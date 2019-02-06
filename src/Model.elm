module Model exposing (AlbumEntry, ArtistEntry, GroupingEntry, Model(..), StarredAlbums, State, ViewOptions, init)

import Catalogue exposing (..)
import Dict exposing (Dict)
import Http
import Json.Encode as E
import Messages exposing (Msg(..))
import Set exposing (Set)


type Model
    = Loading E.Value
    | Failure String
    | Success State


type alias State =
    { catalogue : Catalogue
    , artistsByGenre : Dict GenreId (List ArtistEntry)
    , viewOptions : ViewOptions
    , starredAlbums : StarredAlbums
    }


type alias ArtistEntry =
    { artist : Artist
    , albumsNoGrouping : List AlbumEntry
    , albumsByGrouping : List GroupingEntry
    , isVisible : Bool
    }


type alias GroupingEntry =
    { grouping : Grouping
    , albums : List AlbumEntry
    , isVisible : Bool
    }


type alias AlbumEntry =
    { album : Album
    , isVisible : Bool
    , isStarred : Bool
    }


type alias StarredAlbums =
    Set String


type alias ViewOptions =
    { genre : GenreId
    , filter : String
    , archiveVisibility : ArchiveVisibility
    , visibleSources : List Source
    , onlyStarredVisible : Bool
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( Loading flags
    , Http.get
        { url = "data/catalogue.json"
        , expect = Http.expectJson GotCatalogue catalogueDecoder
        }
    )
