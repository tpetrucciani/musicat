module Update exposing (update)

import Catalogue exposing (..)
import Data exposing (..)
import Decoders
import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode
import List.Extra
import Messages exposing (Msg(..), SetViewMsg(..))
import Model exposing (Model(..))
import Ports
import Set exposing (Set)
import String.Normalize
import Utils.Maybe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Loading flags, GotCatalogue result ) ->
            handleGotCatalogue flags result

        ( Success state, SetView setViewMsg ) ->
            handleSetView state setViewMsg

        ( Success state, ToggleStarred albumToToggle ) ->
            handleToggleStarred state albumToToggle

        _ ->
            ( model, Cmd.none )


handleGotCatalogue :
    Json.Encode.Value
    -> Result Http.Error Catalogue
    -> ( Model, Cmd Msg )
handleGotCatalogue flags result =
    let
        flagsResult =
            Json.Decode.decodeValue Decoders.starredAlbums flags
    in
    case ( result, flagsResult ) of
        ( Ok catalogue, Ok starredAlbums ) ->
            ( Success (makeState catalogue starredAlbums), Cmd.none )

        ( Ok catalogue, Err _ ) ->
            ( Success (makeState catalogue Set.empty), Cmd.none )

        ( Err (Http.BadBody err), _ ) ->
            ( Failure err, Cmd.none )

        ( Err _, _ ) ->
            ( Failure "Error", Cmd.none )


handleSetView : State -> SetViewMsg -> ( Model, Cmd Msg )
handleSetView state setViewMsg =
    let
        newViewOptions =
            setView setViewMsg state.viewOptions

        newState =
            updatePageContents { state | viewOptions = newViewOptions }
    in
    ( Success newState, Cmd.none )


handleToggleStarred : State -> CoverPath -> ( Model, Cmd Msg )
handleToggleStarred state coverPath =
    let
        newStarredAlbums =
            if Set.member coverPath state.starredAlbums then
                Set.remove coverPath state.starredAlbums

            else
                Set.insert coverPath state.starredAlbums

        newState =
            updatePageContents { state | starredAlbums = newStarredAlbums }

        encodedNewStarredAlbums =
            Json.Encode.list Json.Encode.string (Set.toList newStarredAlbums)
    in
    ( Success newState, Ports.setLocalStorage encodedNewStarredAlbums )


setView : SetViewMsg -> ViewOptions -> ViewOptions
setView viewMsg viewOptions =
    case viewMsg of
        SetGenre genre ->
            { viewOptions | genre = genre }

        SetFilter filter ->
            { viewOptions | filter = filter }

        SetArchiveVisibility archiveVisibility ->
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


makeState : Catalogue -> StarredAlbums -> State
makeState catalogue starredAlbums =
    { catalogue = catalogue
    , viewOptions =
        { genre = catalogue.config.selectedGenre
        , filter = ""
        , archiveVisibility = OnlyUnarchived
        , visibleSources = catalogue.config.visibleSources
        , onlyStarredVisible = False
        }
    , starredAlbums = starredAlbums
    , byGenre = makeArtistsByGenre catalogue
    , pageContents = []
    }
        |> updatePageContents


type alias EntryWithAlbum =
    { genre : Genre
    , artist : Artist
    , grouping : Maybe Grouping
    , album : AlbumEntry
    }


makeArtistsByGenre : Catalogue -> Dict GenreId (List ArtistEntry)
makeArtistsByGenre catalogue =
    let
        albumToEntries : Album -> List EntryWithAlbum
        albumToEntries album =
            album.entries
                |> List.map
                    (\{ genre, artist, grouping } ->
                        EntryWithAlbum
                            genre
                            artist
                            grouping
                            { album = album, isVisible = True, isStarred = False }
                    )

        gatherByArtistAndGrouping : List EntryWithAlbum -> List ArtistEntry
        gatherByArtistAndGrouping entries =
            entries
                |> List.Extra.gatherEqualsBy .artist
                |> List.map (toKeyValue .artist)
                |> List.map gatherByGrouping
                |> List.sortBy (.artist >> .sortKey)

        gatherByGrouping : ( Artist, List EntryWithAlbum ) -> ArtistEntry
        gatherByGrouping ( artist, entries ) =
            let
                byGrouping : List ( Maybe Grouping, List AlbumEntry )
                byGrouping =
                    entries
                        |> List.Extra.gatherEqualsBy .grouping
                        |> List.map (toKeyValue .grouping)
                        |> List.map (Tuple.mapSecond (List.map .album))

                albumsNoGrouping =
                    byGrouping
                        |> List.filter (Utils.Maybe.isNothing << Tuple.first)
                        |> List.map Tuple.second
                        |> List.concat

                albumsByGrouping =
                    byGrouping
                        |> List.filterMap
                            (\( g, a ) ->
                                case g of
                                    Just g_ ->
                                        Just
                                            { grouping = g_
                                            , albums = a
                                            , isVisible = True
                                            }

                                    Nothing ->
                                        Nothing
                            )
                        |> List.sortBy (.grouping >> .sortKey)
            in
            { artist = artist
            , albumsNoGrouping = albumsNoGrouping
            , albumsByGrouping = albumsByGrouping
            , isVisible = True
            }

        toKeyValue : (a -> b) -> ( a, List a ) -> ( b, List a )
        toKeyValue f ( x, xs ) =
            ( f x, x :: xs )
    in
    catalogue.albums
        |> List.concatMap albumToEntries
        |> List.Extra.gatherEqualsBy .genre
        |> List.map (toKeyValue (.genre >> .id))
        |> List.map (Tuple.mapSecond gatherByArtistAndGrouping)
        |> Dict.fromList


{-| Recompute the `pageContents` field using the other fields.
-}
updatePageContents : State -> State
updatePageContents state =
    let
        pageContents =
            Dict.get state.viewOptions.genre state.byGenre
                |> Maybe.withDefault []
                |> setVisibility state
    in
    { state | pageContents = pageContents }


setVisibility : State -> List ArtistEntry -> List ArtistEntry
setVisibility state a =
    let
        artistFilter =
            makeArtistFilter state.viewOptions

        albumFilter =
            makeAlbumFilter state.viewOptions state.starredAlbums

        aux : ArtistEntry -> ArtistEntry
        aux { artist, albumsNoGrouping, albumsByGrouping } =
            let
                albumsNoGrouping_ =
                    List.map aux2 albumsNoGrouping

                albumsByGrouping_ =
                    List.map aux3 albumsByGrouping

                isVisible_ =
                    artistFilter artist
                        && (List.any .isVisible albumsNoGrouping_
                                || List.any .isVisible albumsByGrouping_
                           )
            in
            { artist = artist
            , isVisible = isVisible_
            , albumsNoGrouping = albumsNoGrouping_
            , albumsByGrouping = albumsByGrouping_
            }

        aux2 : AlbumEntry -> AlbumEntry
        aux2 { album } =
            { album = album
            , isVisible = albumFilter album
            , isStarred = Set.member album.cover state.starredAlbums
            }

        aux3 : GroupingEntry -> GroupingEntry
        aux3 { grouping, albums } =
            let
                albums_ =
                    List.map aux2 albums
            in
            { grouping = grouping
            , isVisible = List.any .isVisible albums_
            , albums = albums_
            }
    in
    List.map aux a


makeArtistFilter : ViewOptions -> (Artist -> Bool)
makeArtistFilter viewOptions =
    if String.isEmpty viewOptions.filter then
        always True

    else
        let
            normalized =
                String.toLower
                    (String.Normalize.removeDiacritics viewOptions.filter)
        in
        Catalogue.artistMatchesFilter normalized


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
            Utils.Maybe.isJust album.spotify

        Qobuz ->
            Utils.Maybe.isJust album.qobuz

        Missing ->
            not album.local
                && not (Utils.Maybe.isJust album.spotify)
                && not (Utils.Maybe.isJust album.qobuz)
