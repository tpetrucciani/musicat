module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, div, img, input, text)
import Html.Attributes exposing (class, href, id, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, bool, field, list, map, map2, map8, maybe, null, oneOf, string, succeed)
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
    , genres : List Genre
    , albumsByGenre : Dict Genre (Dict Artist (List Album))
    , viewOptions : ViewOptions
    }


type alias Catalogue =
    { albums : List Album
    }


type alias Album =
    { cover : CoverPath
    , entries : List Entry
    , spotify : Maybe SpotifyId
    , qobuz : Maybe QobuzId
    , local : Bool
    , archived : Bool
    , booklet : Maybe BookletPath
    , comment : CommentString
    }


type alias Entry =
    { genre : Genre
    , artist : Artist
    }


type alias CoverPath =
    String


type alias BookletPath =
    String


type alias SpotifyId =
    String


type alias QobuzId =
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
    , filterFunction : String -> Bool
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
    map8 Album
        (field "cover" string)
        (field "entries" (list entryDecoder))
        (maybe (field "spotify" string))
        (maybe (field "qobuz" string))
        (oneOf [ field "local" bool, succeed False ])
        (oneOf [ field "archived" bool, succeed False ])
        (maybe (field "booklet" string))
        (field "comment" string)


entryDecoder : Decoder Entry
entryDecoder =
    map2 Entry (field "genre" string) (field "artist" string)



-- UPDATE


type Msg
    = GotCatalogue (Result Http.Error Catalogue)
    | SetView SetViewMsg


type SetViewMsg
    = SetGenre Genre
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
                SetView viewMsg ->
                    let
                        newViewOptions =
                            setView viewMsg state.viewOptions

                        newState =
                            { state | viewOptions = newViewOptions }
                    in
                    ( Success newState, Cmd.none )

                _ ->
                    ( model, Cmd.none )


setView : SetViewMsg -> ViewOptions -> ViewOptions
setView viewMsg viewOptions =
    case viewMsg of
        SetGenre genre ->
            { viewOptions | currentGenre = genre }

        ChangeFilter filter ->
            { viewOptions
                | searchField = filter
                , filterFunction = makeFilterFunction filter
            }


makeFilterFunction : String -> (String -> Bool)
makeFilterFunction searchField =
    if String.isEmpty searchField then
        \s -> True

    else
        let
            normalized =
                String.toLower (String.Normalize.removeDiacritics searchField)
        in
        \s ->
            let
                ss =
                    String.toLower (String.Normalize.removeDiacritics s)
            in
            String.startsWith normalized ss


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

        albumsByGenre =
            initDict genres dictForGenre

        filteredByGenre =
            albumsByGenre
                |> Dict.map
                    (\genre albumsByArtist ->
                        Dict.filter
                            (\_ albums -> not (List.isEmpty albums))
                            albumsByArtist
                    )
    in
    { catalogue = catalogue
    , genres = Set.toList genres
    , albumsByGenre = filteredByGenre
    , viewOptions = { currentGenre = "Classical", searchField = "", filterFunction = \s -> True }
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
                    :: (state.albumsByGenre
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
            a [ onClick (SetView (SetGenre g)) ] [ text (g ++ " ") ]
    in
    div [] (List.map displayGenre state.genres)


displaySearchBar : State -> Html Msg
displaySearchBar state =
    div []
        [ input
            [ placeholder "Filter"
            , value state.viewOptions.searchField
            , onInput (SetView << ChangeFilter)
            , Html.Attributes.autofocus True
            , Html.Attributes.type_ "search"
            ]
            []
        ]


displayArtist : State -> ( Artist, List Album ) -> Html Msg
displayArtist state ( artist, albums ) =
    let
        contents =
            if state.viewOptions.filterFunction artist then
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
        [ img
            [ src ("data/covers/" ++ album.cover)
            , Html.Attributes.width 200
            , Html.Attributes.height 200
            ]
            []
        ]
