module Update exposing (update)

import Catalogue exposing (..)
import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode as E
import List.Extra
import Messages exposing (..)
import Model exposing (..)
import Ports
import Set exposing (Set)
import Utils.Maybe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading flags ->
            case msg of
                GotCatalogue result ->
                    case result of
                        Ok catalogue ->
                            ( makeModel catalogue flags, Cmd.none )

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

                        encodedNewStarredAlbums =
                            E.list E.string (Set.toList newStarredAlbums)
                    in
                    ( Success newState, Ports.setLocalStorage encodedNewStarredAlbums )

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


makeModel : Catalogue -> E.Value -> Model
makeModel catalogue flags =
    case Json.Decode.decodeValue (Json.Decode.list Json.Decode.string) flags of
        Ok starredAlbums ->
            Success (makeState catalogue starredAlbums)

        Err e ->
            Success (makeState catalogue [])


makeState : Catalogue -> List String -> State
makeState catalogue starredAlbums =
    { catalogue = catalogue
    , artistsByGenre = makeArtistsByGenre catalogue
    , viewOptions =
        { genre = catalogue.config.selectedGenre
        , filter = ""
        , archiveVisibility = OnlyUnarchived
        , visibleSources = catalogue.config.visibleSources
        , onlyStarredVisible = False
        }
    , starredAlbums = Set.fromList starredAlbums
    }


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
