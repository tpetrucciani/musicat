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
    , starredAlbums : StarredAlbums
    }


type alias AlbumsForGenreAndArtist =
    ( List Album, List ( Grouping, List Album ) )


type alias StarredAlbums =
    Set String


type alias ViewOptions =
    { genre : GenreId
    , filter : String
    , archiveVisibility : ArchiveVisibility
    , visibleSources : List Source
    , onlyStarredVisible : Bool
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
        { url = "data/catalogue.json"
        , expect = Http.expectJson GotCatalogue catalogueDecoder
        }
    )



-- UPDATE


type Msg
    = GotCatalogue (Result Http.Error Catalogue)
    | SetView SetViewMsg
    | ToggleStarred String


type SetViewMsg
    = SetGenre GenreId
    | ChangeFilter String
    | ChangeArchiveVisibility ArchiveVisibility
    | ToggleSourceVisibility Source
    | ToggleOnlyStarredVisible


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

                ToggleStarred id ->
                    let
                        newStarredAlbums =
                            if Set.member id state.starredAlbums then
                                Set.remove id state.starredAlbums

                            else
                                Set.insert id state.starredAlbums

                        newState =
                            { state | starredAlbums = newStarredAlbums }
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

        ToggleOnlyStarredVisible ->
            { viewOptions
                | onlyStarredVisible = not viewOptions.onlyStarredVisible
            }


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
        , onlyStarredVisible = False
        }
    , starredAlbums = Set.fromList []
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
            [ Html.div [ id "top" ] []
            , Html.header []
                [ Html.nav []
                    [ displaySearchBar state
                    , displayGenres state
                    , displaySourceVisibilitySelector state
                    , displayArchiveVisibilitySelector state
                    , displayStarVisibilitySelector state
                    ]
                , displayArtistList visibleAlbumsByArtist
                ]
            , Html.main_ []
                (List.map (displayArtist state) visibleAlbumsByArtist)
            , Html.footer []
                [ text
                    ("Colours based on the Atom One themes. "
                        ++ "Fonts: Source Sans Pro, "
                    )
                , a [ href "https://fontawesome.com/license/free" ]
                    [ text "Font Awesome" ]
                , text "."
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
            List.filter (makeAlbumFilter state.viewOptions state.starredAlbums)
                albums

        filterGrouping ( grouping, albums ) =
            let
                filtered =
                    aux albums
            in
            case filtered of
                [] ->
                    Nothing

                _ ->
                    Just ( grouping, filtered )
    in
    state.albumsByGenreAndArtist
        |> Dict.get state.viewOptions.genre
        |> Maybe.withDefault []
        |> List.filter (makeArtistFilter state.viewOptions)
        |> List.map
            (\( artist, ( albumsNoGrp, albumsByGrp ) ) ->
                ( artist
                , ( aux albumsNoGrp
                  , List.filterMap filterGrouping albumsByGrp
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


makeAlbumFilter : ViewOptions -> StarredAlbums -> (Album -> Bool)
makeAlbumFilter viewOptions starredAlbums album =
    matchesArchiveVisibility album viewOptions.archiveVisibility
        && List.any (matchesSourceVisibility album) viewOptions.visibleSources
        && (not viewOptions.onlyStarredVisible
                || Set.member album.cover starredAlbums
           )


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
        genres =
            List.sortBy .sortKey state.catalogue.genres

        mkRadio : Genre -> Html Msg
        mkRadio g =
            radio
                (SetView <| SetGenre g.id)
                (String.toLower g.name)
                (state.viewOptions.genre == g.id)
    in
    div [ id "genres" ] [ Html.fieldset [] (List.map mkRadio genres) ]


displaySearchBar : State -> Html Msg
displaySearchBar state =
    div [ id "searchbar" ]
        [ input
            [ placeholder "type to filter…"
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
                (String.toLower (archiveVisibilityName visibility))
                (state.viewOptions.archiveVisibility == visibility)
    in
    div [ id "archive-selector" ]
        [ Html.fieldset []
            (List.map mkRadio [ OnlyUnarchived, OnlyArchived, Both ])
        ]


displaySourceVisibilitySelector : State -> Html Msg
displaySourceVisibilitySelector state =
    let
        source2checkbox s =
            checkbox
                (SetView <| ToggleSourceVisibility s)
                (String.toLower (sourceName s))
                (List.member s state.viewOptions.visibleSources)

        checkboxes =
            List.map source2checkbox [ Local, Qobuz, Spotify, Missing ]
    in
    div [ id "source-selector" ] [ Html.fieldset [] checkboxes ]


displayStarVisibilitySelector : State -> Html Msg
displayStarVisibilitySelector state =
    let
        c =
            checkbox (SetView ToggleOnlyStarredVisible)
                "only starred"
                state.viewOptions.onlyStarredVisible
    in
    div [ id "star-selector" ] [ Html.fieldset [] [ c ] ]


archiveVisibilityName : ArchiveVisibility -> String
archiveVisibilityName visibility =
    case visibility of
        OnlyUnarchived ->
            "Unarchived"

        OnlyArchived ->
            "Archived"

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
        , Html.span [] [ text name ]
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


displayArtist : State -> ( Artist, AlbumsForGenreAndArtist ) -> Html Msg
displayArtist state ( artist, ( albumsNoGrp, albumsByGrp ) ) =
    let
        contents =
            [ a [ class "artist-name", id artist.id, href "#top" ]
                [ text (artistName state.catalogue artist) ]
            , div
                [ class "grouping-links" ]
                (List.map (displayGroupingLink << Tuple.first) albumsByGrp)
            , div [ class "album-container" ]
                (List.map (displayAlbum state.starredAlbums) albumsNoGrp
                    ++ List.map
                        (displayGrouping state.starredAlbums artist)
                        albumsByGrp
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


displayGrouping : StarredAlbums -> Artist -> ( Grouping, List Album ) -> Html Msg
displayGrouping starredAlbums artist ( grouping, albums ) =
    div [ class "grouping" ]
        (div [ class "grouping-name" ]
            [ a [ id (makeId grouping.name), href ("#" ++ artist.id) ]
                [ text grouping.name ]
            ]
            :: List.map (displayAlbum starredAlbums) albums
        )


displayAlbum : StarredAlbums -> Album -> Html Msg
displayAlbum starredAlbums album =
    let
        imageLink =
            case album.spotify of
                Just id ->
                    [ href ("spotify:album:" ++ id) ]

                Nothing ->
                    []

        starAttrs =
            [ class "star", onClick (ToggleStarred album.cover) ]

        star =
            if Set.member album.cover starredAlbums then
                div (starAttrs ++ [ class "starred" ])
                    [ img [ src "resources/star-solid.svg" ] [] ]

            else
                div starAttrs [ img [ src "resources/star-regular.svg" ] [] ]
    in
    div
        [ class "album" ]
        [ a imageLink
            [ img
                [ src ("data/covers/" ++ album.cover)
                , Html.Attributes.width 220
                , Html.Attributes.height 220
                , class "cover"
                ]
                []
            ]
        , star
        , div [ class "icon-bar" ] (putIcons album)
        ]


putIcons : Album -> List (Html Msg)
putIcons album =
    let
        localIcon =
            if album.local then
                [ div [ class "icon", class "local" ]
                    [ a [ title "The album is available locally" ]
                        [ text "Local" ]
                    ]
                ]

            else
                []

        qobuzIcon =
            case album.qobuz of
                Just id ->
                    [ div [ class "icon", class "qobuz" ]
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
                    [ div [ class "icon", class "spotify" ]
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
                    [ div [ class "icon", class "booklet" ]
                        [ a
                            [ title "Show album booklet"
                            , href ("data/booklets/" ++ path)
                            , Html.Attributes.target "_blank"
                            ]
                            [ text "Booklet" ]
                        ]
                    ]

                Nothing ->
                    []

        archivedIcon =
            if album.archived then
                [ div [ class "icon", class "archived" ]
                    [ a [ title "The album is archived" ]
                        [ text "Archived" ]
                    ]
                ]

            else
                []
    in
    List.concat [ localIcon, qobuzIcon, spotifyIcon, bookletIcon, archivedIcon ]
