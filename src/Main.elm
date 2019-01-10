module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, div, img, input, text)
import Html.Attributes exposing (class, href, id, placeholder, src, title, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, bool, field, list, map, map2, map8, maybe, null, oneOf, string, succeed)
import NaturalOrdering
import Set exposing (Set)
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
    , albumsByGenre : Dict Genre (List ( Artist, List Album ))
    , viewOptions : ViewOptions
    }


type alias Catalogue =
    { config : Config
    , albums : List Album
    }


type alias Config =
    { selectedGenre : Genre }


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
    { genre : Genre
    , filter : String
    , archiveVisibility : ArchiveVisibility
    , visibleSources : List Source
    }


type ArchiveVisibility
    = OnlyUnarchived
    | OnlyArchived
    | Both


type Source
    = Local
    | Spotify
    | Qobuz
    | Missing


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
    map2 Catalogue
        (field "config" configDecoder)
        (field "albums" (list albumDecoder))


configDecoder : Decoder Config
configDecoder =
    map Config (field "selectedGenre" string)


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
    | ChangeArchiveVisibility ArchiveVisibility
    | ToggleSourceVisibility Source


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
            { viewOptions | genre = genre }

        ChangeFilter filter ->
            { viewOptions | filter = filter }

        ChangeArchiveVisibility archiveVisibility ->
            { viewOptions | archiveVisibility = archiveVisibility }

        ToggleSourceVisibility source ->
            let
                newVisibleSources =
                    if List.member source viewOptions.visibleSources then
                        List.filter ((/=) source) viewOptions.visibleSources

                    else
                        source :: viewOptions.visibleSources
            in
            { viewOptions | visibleSources = newVisibleSources }


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
                |> Set.toList
                |> List.sortWith NaturalOrdering.compare

        albumsForGenreArtist g a =
            entries
                |> List.filter (\e -> e.genre == g && e.artist == a)
                |> List.map .album

        listForGenre g =
            artists
                |> List.map (\a -> ( a, albumsForGenreArtist g a ))

        albumsByGenre =
            initDict genres listForGenre

        filteredByGenre =
            albumsByGenre
                |> Dict.map
                    (\genre albumsByArtist ->
                        List.filter
                            (\( _, albums ) -> not (List.isEmpty albums))
                            albumsByArtist
                    )
    in
    { catalogue = catalogue
    , genres = Set.toList genres
    , albumsByGenre = filteredByGenre
    , viewOptions =
        { genre = catalogue.config.selectedGenre
        , filter = ""
        , archiveVisibility = OnlyUnarchived
        , visibleSources = [ Local, Spotify, Qobuz, Missing ]
        }
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
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    case model of
        Failure err ->
            [ text err ]

        Loading ->
            [ text "Loading..." ]

        Success state ->
            [ Html.header []
                [ displayGenres state
                , displaySearchBar state
                , displayArchiveVisibilitySelector state
                , displaySourceVisibilitySelector state
                ]
            , Html.main_ []
                (List.map displayArtist (getVisibleAlbumsByArtist state))
            , Html.footer [] []
            ]


getVisibleAlbumsByArtist : State -> List ( Artist, List Album )
getVisibleAlbumsByArtist state =
    Dict.get state.viewOptions.genre state.albumsByGenre
        |> Maybe.withDefault []
        |> List.filter (makeArtistFilter state.viewOptions)
        |> List.map
            (\( a, albums ) ->
                ( a, List.filter (makeAlbumFilter state.viewOptions) albums )
            )
        |> List.filter (\( _, albums ) -> not (List.isEmpty albums))


makeArtistFilter : ViewOptions -> (( Artist, List Album ) -> Bool)
makeArtistFilter viewOptions =
    if String.isEmpty viewOptions.filter then
        always True

    else
        let
            normalized =
                String.toLower
                    (String.Normalize.removeDiacritics viewOptions.filter)
        in
        \( a, _ ) ->
            let
                aa =
                    String.toLower (String.Normalize.removeDiacritics a)
            in
            String.startsWith normalized aa


makeAlbumFilter : ViewOptions -> (Album -> Bool)
makeAlbumFilter viewOptions album =
    matchesArchiveVisibility album viewOptions.archiveVisibility
        && List.any (matchesSourceVisibility album) viewOptions.visibleSources


matchesArchiveVisibility : Album -> ArchiveVisibility -> Bool
matchesArchiveVisibility album archiveVisibility =
    case archiveVisibility of
        OnlyUnarchived ->
            not album.archived

        OnlyArchived ->
            album.archived

        Both ->
            True


matchesSourceVisibility : Album -> Source -> Bool
matchesSourceVisibility album source =
    case source of
        Local ->
            album.local

        Spotify ->
            isSome album.spotify

        Qobuz ->
            isSome album.qobuz

        Missing ->
            not album.local
                && not (isSome album.spotify)
                && not (isSome album.qobuz)


isSome : Maybe a -> Bool
isSome x =
    case x of
        Just _ ->
            True

        Nothing ->
            False


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
            , value state.viewOptions.filter
            , onInput (SetView << ChangeFilter)
            , Html.Attributes.autofocus True
            , Html.Attributes.type_ "search"
            ]
            []
        ]


displayArchiveVisibilitySelector : State -> Html Msg
displayArchiveVisibilitySelector state =
    div []
        [ Html.fieldset []
            [ radio (SetView <| ChangeArchiveVisibility OnlyUnarchived)
                "Only unarchived"
                (state.viewOptions.archiveVisibility == OnlyUnarchived)
            , radio (SetView <| ChangeArchiveVisibility OnlyArchived)
                "Only archived"
                (state.viewOptions.archiveVisibility == OnlyArchived)
            , radio (SetView <| ChangeArchiveVisibility Both)
                "Both"
                (state.viewOptions.archiveVisibility == Both)
            ]
        ]


displaySourceVisibilitySelector : State -> Html Msg
displaySourceVisibilitySelector state =
    let
        source2checkbox s =
            checkbox
                (SetView <| ToggleSourceVisibility s)
                (sourceName s)
                (List.member s state.viewOptions.visibleSources)

        checkboxes =
            List.map source2checkbox [ Local, Spotify, Qobuz, Missing ]
    in
    div [] [ Html.fieldset [] checkboxes ]


sourceName : Source -> String
sourceName source =
    case source of
        Local ->
            "Local"

        Spotify ->
            "Spotify"

        Qobuz ->
            "Qobuz"

        Missing ->
            "Missing"


box : String -> msg -> String -> Bool -> Html msg
box type_ msg name isChecked =
    Html.label []
        [ input
            [ Html.Attributes.type_ type_
            , Html.Attributes.checked isChecked
            , onClick msg
            ]
            []
        , text name
        ]


radio : msg -> String -> Bool -> Html msg
radio =
    box "radio"


checkbox : msg -> String -> Bool -> Html msg
checkbox =
    box "checkbox"


displayArtist : ( Artist, List Album ) -> Html Msg
displayArtist ( artist, albums ) =
    let
        contents =
            [ a [ class "artist-name", id artist ] [ text artist ]
            , div [] (List.map displayAlbum albums)
            ]
    in
    div [ class "artist" ] contents


displayAlbum : Album -> Html Msg
displayAlbum album =
    div
        [ class "album"
        ]
        [ img
            [ src ("data/covers/" ++ album.cover)
            , Html.Attributes.width 200
            , Html.Attributes.height 200
            ]
            []
        , div [ class "icon-bar" ] (putIcons album)
        ]


putIcons : Album -> List (Html Msg)
putIcons album =
    (if album.local then
        [ a [ title "Local" ] [ div [ class "icon-local", class "icon" ] [] ] ]

     else
        []
    )
        ++ (case album.spotify of
                Just id ->
                    [ a [ title "Spotify", href ("spotify:album:" ++ id) ]
                        [ div [ class "icon-spotify", class "icon" ] [] ]
                    ]

                Nothing ->
                    []
           )
        ++ (case album.qobuz of
                Just id ->
                    [ a [ title "Qobuz", href ("qobuzapp://album/" ++ id) ]
                        [ div [ class "icon-qobuz", class "icon" ] [] ]
                    ]

                Nothing ->
                    []
           )
        ++ (if album.archived then
                [ a [ title "Archived" ]
                    [ div [ class "icon-archived", class "icon" ] [] ]
                ]

            else
                []
           )
