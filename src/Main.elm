module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, div, img, input, text)
import Html.Attributes exposing (class, href, id, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, list, map, map2, map4, string)
import Set
import String.Normalize



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Loading
    | Failure String
    | Success State


type alias State =
    { catalogue : Catalogue
    , byGenre : Dict Genre (Dict Artist (List Album))
    , genres : List Genre
    , viewOptions : ViewOptions
    }


type alias Catalogue =
    { albums : List Album
    }


type alias Album =
    { cover : CoverPath
    , spotify : SpotifyId
    , entries : List Entry
    , comment : CommentString
    }


type alias Entry =
    { genre : Genre
    , artist : Artist
    }


type alias CoverPath =
    String


type alias SpotifyId =
    String


type alias Genre =
    String


type alias Artist =
    String


type alias CommentString =
    String


type alias ViewOptions =
    { currentGenre : Genre
    , searchField : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "/data/catalogue"
        , expect = Http.expectJson GotCatalogue catalogueDecoder
        }
    )


catalogueDecoder : Decoder Catalogue
catalogueDecoder =
    map Catalogue (field "albums" (list albumDecoder))


albumDecoder : Decoder Album
albumDecoder =
    map4 Album
        (field "cover" string)
        (field "spotify" string)
        (field "entries" (list entryDecoder))
        (field "comment" string)


entryDecoder : Decoder Entry
entryDecoder =
    map2 Entry (field "genre" string) (field "artist" string)



-- UPDATE


type Msg
    = GotCatalogue (Result Http.Error Catalogue)
    | SetGenre Genre
    | ChangeFilter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading ->
            case msg of
                GotCatalogue result ->
                    case result of
                        Ok catalogue ->
                            ( Success (makeState catalogue), Cmd.none )

                        Err (Http.BadBody err) ->
                            ( Failure err, Cmd.none )

                        Err _ ->
                            ( Failure "Error", Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Failure _ ->
            ( model, Cmd.none )

        Success state ->
            case msg of
                SetGenre genre ->
                    let
                        oldViewOptions =
                            state.viewOptions

                        newViewOptions =
                            { oldViewOptions | currentGenre = genre }

                        newState =
                            { state | viewOptions = newViewOptions }
                    in
                    ( Success newState, Cmd.none )

                ChangeFilter filter ->
                    let
                        oldViewOptions =
                            state.viewOptions

                        newViewOptions =
                            { oldViewOptions | searchField = filter }

                        newState =
                            { state | viewOptions = newViewOptions }
                    in
                    ( Success newState, Cmd.none )

                _ ->
                    ( model, Cmd.none )


makeState : Catalogue -> State
makeState catalogue =
    let
        getEntries album =
            List.map
                (\e -> { genre = e.genre, artist = e.artist, album = album })
                album.entries

        entries =
            List.concatMap getEntries catalogue.albums

        getField f =
            entries
                |> List.map f
                |> Set.fromList

        genres =
            getField .genre

        artists =
            getField .artist

        albumsForGenreArtist g a =
            entries
                |> List.filter (\e -> e.genre == g && e.artist == a)
                |> List.map .album

        dictForGenre g =
            initDict artists (albumsForGenreArtist g)

        byGenre =
            initDict genres dictForGenre

        filteredByGenre =
            byGenre
                |> Dict.map
                    (\genre albumsByArtist ->
                        Dict.filter
                            (\_ albums -> not (List.isEmpty albums))
                            albumsByArtist
                    )
    in
    { catalogue = catalogue
    , byGenre = filteredByGenre
    , genres = Set.toList genres
    , viewOptions = { currentGenre = "Classical", searchField = "" }
    }


initDict : Set.Set comparable -> (comparable -> a) -> Dict comparable a
initDict s f =
    Set.foldl (\x -> Dict.insert x (f x)) Dict.empty s



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Album catalogue"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    case model of
        Failure err ->
            text err

        Loading ->
            text "Loading..."

        Success state ->
            div []
                (displayGenres state
                    :: displaySearchBar state
                    :: (state.byGenre
                            |> Dict.get state.viewOptions.currentGenre
                            |> Maybe.withDefault Dict.empty
                            |> Dict.toList
                            |> List.map (displayArtist state)
                       )
                )


displayGenres : State -> Html Msg
displayGenres state =
    let
        displayGenre : Genre -> Html Msg
        displayGenre g =
            a [ onClick (SetGenre g) ] [ text (g ++ " ") ]
    in
    div [] (List.map displayGenre state.genres)


displaySearchBar : State -> Html Msg
displaySearchBar state =
    div []
        [ input
            [ placeholder "Filter"
            , value state.viewOptions.searchField
            , onInput ChangeFilter
            ]
            []
        ]


displayArtist : State -> ( Artist, List Album ) -> Html Msg
displayArtist state ( artist, albums ) =
    let
        normalizedArtist =
            String.toLower (String.Normalize.removeDiacritics artist)

        contents =
            if
                String.startsWith
                    (String.toLower state.viewOptions.searchField)
                    normalizedArtist
            then
                [ a [ id artist ] [ text artist ]
                , div [] (List.map displayAlbum albums)
                ]

            else
                []
    in
    div [] contents


displayAlbum : Album -> Html Msg
displayAlbum album =
    div [ class "album" ]
        [ a [ href ("spotify:album:" ++ album.spotify) ]
            [ img [ src ("data/covers/" ++ album.cover) ] [] ]
        ]
