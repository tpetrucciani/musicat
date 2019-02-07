module Data exposing (Album, AlbumEntry, ArchiveVisibility(..), Artist, ArtistEntry, ArtistId, BookletPath, Catalogue, Config, CoverPath, Entry, Genre, GenreId, Grouping, GroupingEntry, QobuzId, Source(..), SpotifyId, StarredAlbums, State, ViewOptions)

import Dict exposing (Dict)
import Set exposing (Set)



-- BASE TYPES


type alias GenreId =
    String


type alias ArtistId =
    String


type alias CoverPath =
    String


type alias QobuzId =
    String


type alias SpotifyId =
    String


type alias BookletPath =
    String


{-| The different kinds of sources an album can be available from
(Missing: not available from any source)
-}
type Source
    = Local
    | Spotify
    | Qobuz
    | Missing


type ArchiveVisibility
    = OnlyUnarchived
    | OnlyArchived
    | Both



-- CATALOGUE


type alias Catalogue =
    { genres : List Genre
    , artists : List Artist
    , albums : List Album
    , config : Config
    }


type alias Genre =
    { id : GenreId
    , name : String
    , sortKey : String
    }


type alias Artist =
    { id : ArtistId
    , name : String
    , shortName : String
    , sortKey : String
    }


type alias Album =
    { cover : CoverPath
    , entries : List Entry
    , qobuz : Maybe QobuzId
    , spotify : Maybe SpotifyId
    , local : Bool
    , archived : Bool
    , booklet : Maybe BookletPath
    }


type alias Entry =
    { genre : Genre
    , artist : Artist
    , grouping : Maybe Grouping
    }


type alias Grouping =
    { name : String
    , sortKey : String
    }


type alias Config =
    { selectedGenre : GenreId
    , visibleSources : List Source
    }



-- APPLICATION STATE (for successfully loaded catalogues)


{-| Represents the state of the application when it has loaded the catalogue
successfully. The fields are:

  - `catalogue`: the catalogue (loaded and never modified);
  - `viewOptions`: current view options e.g. contents of search box;
  - `starredAlbums`: stores the currently starred albums;
  - `byGenre`: the albums arranged by genre, artist, and grouping;
  - `pageContents`: the artists (and groupings and albums for each)
    with information on whether they are currently visible or not.

The last two are computed from the first three and stored to simplify the view.

-}
type alias State =
    { catalogue : Catalogue
    , viewOptions : ViewOptions
    , starredAlbums : StarredAlbums
    , byGenre : Dict GenreId (List ArtistEntry)
    , pageContents : List ArtistEntry
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


type alias ViewOptions =
    { genre : GenreId
    , filter : String
    , archiveVisibility : ArchiveVisibility
    , visibleSources : List Source
    , onlyStarredVisible : Bool
    }


type alias StarredAlbums =
    Set String
