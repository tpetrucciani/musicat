module View exposing (view)

import Browser
import Catalogue exposing (..)
import Data exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, a, div, img, input, text)
import Html.Attributes exposing (class, href, id, placeholder, src, title, value)
import Html.Events exposing (onClick, onInput)
import Messages exposing (Msg(..), SetViewMsg(..))
import Model exposing (..)
import Set exposing (Set)
import String.Normalize


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

        Loading _ ->
            [ text "Loading..." ]

        Success state ->
            let
                visibleAlbumsByArtist =
                    state.pageContents
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


displayArtistList : List ArtistEntry -> Html Msg
displayArtistList albumsByArtist =
    let
        artists =
            List.filter .isVisible albumsByArtist
                |> List.map .artist

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
            , onInput (SetView << SetFilter)
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
                (SetView <| SetArchiveVisibility visibility)
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


displayArtist : State -> ArtistEntry -> Html Msg
displayArtist state { artist, isVisible, albumsNoGrouping, albumsByGrouping } =
    let
        contents =
            [ a [ class "artist-name", id artist.id, href "#top" ]
                [ text (artistName state.catalogue artist) ]
            , div
                [ class "grouping-links" ]
                (albumsByGrouping
                    |> List.filter .isVisible
                    |> List.map (.grouping >> displayGroupingLink artist)
                )
            , div [ class "album-container" ]
                (List.map displayAlbum albumsNoGrouping
                    ++ List.map
                        (displayGrouping artist)
                        albumsByGrouping
                )
            ]
    in
    div (withHiddenUnless isVisible [ class "artist" ]) contents


makeId : String -> String
makeId =
    String.Normalize.url


displayGroupingLink : Artist -> Grouping -> Html Msg
displayGroupingLink artist grouping =
    a
        [ class "grouping-link"
        , href ("#" ++ makeId (artist.id ++ "-" ++ grouping.name))
        ]
        [ text grouping.name ]


withHiddenUnless : Bool -> List (Html.Attribute msg) -> List (Html.Attribute msg)
withHiddenUnless condition classes =
    if condition then
        classes

    else
        class "hidden" :: classes


displayGrouping : Artist -> GroupingEntry -> Html Msg
displayGrouping artist { grouping, isVisible, albums } =
    div (withHiddenUnless isVisible [ class "grouping" ])
        (div [ class "grouping-name" ]
            [ a
                [ id (makeId (artist.id ++ "-" ++ grouping.name))
                , href ("#" ++ artist.id)
                ]
                [ text grouping.name ]
            ]
            :: List.map displayAlbum albums
        )


displayAlbum : AlbumEntry -> Html Msg
displayAlbum { album, isVisible, isStarred } =
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
            if isStarred then
                div (starAttrs ++ [ class "starred" ])
                    [ img [ src "resources/star-solid.svg" ] [] ]

            else
                div starAttrs [ img [ src "resources/star-regular.svg" ] [] ]
    in
    div
        (withHiddenUnless isVisible [ class "album" ])
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
                            , href ("qobuzapp://" ++ id)
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
                            , href id
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
