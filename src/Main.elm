module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Browser
import Catalogue exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, a, div, img, input, text)
import Html.Attributes exposing (class, href, id, placeholder, src, title, value)
import Html.Events exposing (onClick, onInput)
import Http
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
    , albumsByGenreAndArtist :
        Dict GenreId (List ( Artist, AlbumsForGenreAndArtist ))
    , viewOptions : ViewOptions
    }


type alias AlbumsForGenreAndArtist =
    ( List Album, List ( Grouping, List Album ) )


type alias ViewOptions =
    { genre : GenreId
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
        { url = "/data/catalogue.json"
        , expect = Http.expectJson GotCatalogue catalogueDecoder
        }
    )



-- UPDATE


type Msg
    = GotCatalogue (Result Http.Error Catalogue)
    | SetView SetViewMsg


type SetViewMsg
    = SetGenre GenreId
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
        artists =
            List.sortBy .sortKey catalogue.artists

        albumsByGenreAndArtist : Dict GenreId (List ( Artist, AlbumsForGenreAndArtist ))
        albumsByGenreAndArtist =
            catalogue.genres
                |> List.sortBy .sortKey
                |> List.map .id
                |> initAssocList albumsByArtistForGenre
                |> Dict.fromList

        albumsByArtistForGenre : GenreId -> List ( Artist, AlbumsForGenreAndArtist )
        albumsByArtistForGenre genre =
            artists
                |> initAssocList (albumsForGenreAndArtist genre)

        albumsForGenreAndArtist : GenreId -> Artist -> AlbumsForGenreAndArtist
        albumsForGenreAndArtist genre artist =
            let
                matchesGenreArtist : GenreId -> Artist -> Entry -> Bool
                matchesGenreArtist g a entry =
                    entry.genre.id == g && entry.artist == a

                matchesGrouping : Maybe Grouping -> Album -> Bool
                matchesGrouping grouping album =
                    List.any (\e -> e.grouping == grouping) album.entries

                albums : List Album
                albums =
                    catalogue.albums
                        |> List.filter
                            (\a ->
                                List.any (matchesGenreArtist genre artist)
                                    a.entries
                            )

                groupings : List Grouping
                groupings =
                    List.concatMap
                        (\a -> List.filterMap (\e -> e.grouping) a.entries)
                        albums
                        |> List.sortBy .sortKey
                        |> deduplicateSortedList

                albumsForGrouping : Maybe Grouping -> List Album
                albumsForGrouping grouping =
                    List.filter (matchesGrouping grouping) albums
            in
            ( albumsForGrouping Nothing
            , List.map (\g -> ( g, albumsForGrouping (Just g) )) groupings
            )
    in
    { catalogue = catalogue
    , albumsByGenreAndArtist = albumsByGenreAndArtist
    , viewOptions =
        { genre = catalogue.config.selectedGenre
        , filter = ""
        , archiveVisibility = OnlyUnarchived
        , visibleSources = [ Local, Spotify, Qobuz, Missing ]
        }
    }


deduplicateSortedList : List a -> List a
deduplicateSortedList l =
    case l of
        x :: ((y :: tail) as ll) ->
            if x == y then
                deduplicateSortedList ll

            else
                x :: deduplicateSortedList ll

        _ ->
            l


initAssocList : (a -> b) -> List a -> List ( a, b )
initAssocList f =
    List.map (\a -> ( a, f a ))


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
            let
                visibleAlbumsByArtist =
                    getVisibleAlbumsByArtist state
            in
            [ Html.header [ id "top" ]
                [ displayGenres state
                , displaySearchBar state
                , displayArchiveVisibilitySelector state
                , displaySourceVisibilitySelector state
                , displayArtistList visibleAlbumsByArtist
                ]
            , Html.main_ []
                (List.map (displayArtist state.catalogue) visibleAlbumsByArtist)
            , Html.footer []
                [ text
                    ("Colours based on the Atom One themes. "
                        ++ "Font: Source Sans Pro."
                    )
                ]
            ]


displayArtistList : List ( Artist, AlbumsForGenreAndArtist ) -> Html Msg
displayArtistList albumsByArtist =
    let
        artists =
            List.map Tuple.first albumsByArtist

        firstLetter artist =
            String.left 1 artist.sortKey

        artistsAndLetters =
            List.foldl
                (\artist ( list, letter ) ->
                    let
                        l =
                            firstLetter artist
                    in
                    ( ( artist, letter /= l ) :: list, l )
                )
                ( [], "" )
                artists
                |> Tuple.first
                |> List.reverse

        artistLinks =
            List.map
                (\( artist, showLetter ) ->
                    if showLetter then
                        Html.span []
                            [ makeLetter (firstLetter artist), makeName artist ]

                    else
                        makeName artist
                )
                artistsAndLetters

        makeLetter letter =
            Html.span [ class "artist-list-letter" ]
                [ text (String.toUpper letter) ]

        makeName artist =
            Html.span [ class "artist-list-name" ]
                [ a [ href ("#" ++ artist.id) ] [ text artist.shortName ] ]
    in
    div [ class "artist-list" ] artistLinks


getVisibleAlbumsByArtist : State -> List ( Artist, AlbumsForGenreAndArtist )
getVisibleAlbumsByArtist state =
    let
        aux albums =
            albums
                |> List.filter (makeAlbumFilter state.viewOptions)
    in
    state.albumsByGenreAndArtist
        |> Dict.get state.viewOptions.genre
        |> Maybe.withDefault []
        |> List.filter (makeArtistFilter state.viewOptions)
        |> List.map
            (\( artist, ( albumsNoGrp, albumsByGrp ) ) ->
                ( artist
                , ( aux albumsNoGrp
                  , List.map (\( g, a ) -> ( g, aux a )) albumsByGrp
                  )
                )
            )
        |> List.filter
            (\( _, ( albumsNoGrp, albumsByGrp ) ) ->
                not (List.isEmpty albumsNoGrp)
                    || List.any (not << List.isEmpty << Tuple.second) albumsByGrp
            )


makeArtistFilter : ViewOptions -> (( Artist, AlbumsForGenreAndArtist ) -> Bool)
makeArtistFilter viewOptions =
    if String.isEmpty viewOptions.filter then
        always True

    else
        let
            normalized =
                String.toLower
                    (String.Normalize.removeDiacritics viewOptions.filter)
        in
        Catalogue.artistMatchesFilter normalized << Tuple.first


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
            isJust album.spotify

        Qobuz ->
            isJust album.qobuz

        Missing ->
            not album.local
                && not (isJust album.spotify)
                && not (isJust album.qobuz)


isJust : Maybe a -> Bool
isJust x =
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
            a [ onClick (SetView (SetGenre g.id)) ] [ text (g.name ++ " ") ]
    in
    div [] (List.map displayGenre state.catalogue.genres)


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
    let
        mkRadio visibility =
            radio
                (SetView <| ChangeArchiveVisibility visibility)
                (archiveVisibilityName visibility)
                (state.viewOptions.archiveVisibility == visibility)
    in
    div []
        [ Html.fieldset []
            (List.map mkRadio [ OnlyUnarchived, OnlyArchived, Both ])
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


archiveVisibilityName : ArchiveVisibility -> String
archiveVisibilityName visibility =
    case visibility of
        OnlyUnarchived ->
            "Only unarchived"

        OnlyArchived ->
            "Only archived"

        Both ->
            "Both"


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


artistName : Catalogue -> Artist -> String
artistName catalogue artist =
    artist.name


displayArtist : Catalogue -> ( Artist, AlbumsForGenreAndArtist ) -> Html Msg
displayArtist catalogue ( artist, ( albumsNoGrp, albumsByGrp ) ) =
    let
        contents =
            [ a [ class "artist-name", id artist.id, href "#top" ]
                [ text (artistName catalogue artist) ]
            , div
                [ class "grouping-links" ]
                (List.map (displayGroupingLink << Tuple.first) albumsByGrp)
            , div [ class "album-container" ]
                (List.map displayAlbum albumsNoGrp
                    ++ List.map (displayGrouping artist) albumsByGrp
                )
            ]
    in
    div [ class "artist" ] contents


makeId : String -> String
makeId =
    String.Normalize.url


displayGroupingLink : Grouping -> Html Msg
displayGroupingLink grouping =
    a [ class "grouping-link", href ("#" ++ makeId grouping.name) ]
        [ text grouping.name ]


displayGrouping : Artist -> ( Grouping, List Album ) -> Html Msg
displayGrouping artist ( grouping, albums ) =
    div [ class "grouping" ]
        (div [ class "grouping-name" ]
            [ a [ id (makeId grouping.name), href ("#" ++ artist.id) ]
                [ text grouping.name ]
            ]
            :: List.map displayAlbum albums
        )


displayAlbum : Album -> Html Msg
displayAlbum album =
    div
        [ class "album"
        ]
        [ img
            [ src ("data/covers/" ++ album.cover)
            , Html.Attributes.width 220
            , Html.Attributes.height 220
            ]
            []
        , div [ class "icon-bar" ] (putIcons album)
        ]


putIcons : Album -> List (Html Msg)
putIcons album =
    let
        localIcon =
            if album.local then
                [ div [ class "icon", class "icon-local" ]
                    [ a [ title "The album is available locally" ]
                        [ text "Local" ]
                    ]
                ]

            else
                []

        qobuzIcon =
            case album.qobuz of
                Just id ->
                    [ div [ class "icon", class "icon-qobuz" ]
                        [ a
                            [ title "Show in Qobuz"
                            , href ("qobuzapp://album/" ++ id)
                            ]
                            [ text "Qobuz" ]
                        ]
                    ]

                Nothing ->
                    []

        spotifyIcon =
            case album.spotify of
                Just id ->
                    [ div [ class "icon", class "icon-spotify" ]
                        [ a
                            [ title "Show in Spotify"
                            , href ("spotify:album:" ++ id)
                            ]
                            [ text "Spotify" ]
                        ]
                    ]

                Nothing ->
                    []

        bookletIcon =
            case album.booklet of
                Just path ->
                    [ div [ class "icon", class "icon-booklet" ]
                        [ a
                            [ title "Show album booklet"
                            , href ("data/booklets/" ++ path)
                            ]
                            [ text "Booklet" ]
                        ]
                    ]

                Nothing ->
                    []

        archivedIcon =
            if album.archived then
                [ div [ class "icon", class "icon-archived" ]
                    [ a [ title "The album is archived" ]
                        [ text "Archived" ]
                    ]
                ]

            else
                []
    in
    List.concat [ localIcon, qobuzIcon, spotifyIcon, bookletIcon, archivedIcon ]
