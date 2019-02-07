module Messages exposing (Msg(..), SetViewMsg(..))

import Data exposing (ArchiveVisibility, Catalogue, GenreId, Source)
import Http


type Msg
    = GotCatalogue (Result Http.Error Catalogue)
    | SetView SetViewMsg
    | ToggleStarred String


type SetViewMsg
    = SetGenre GenreId
    | SetFilter String
    | SetArchiveVisibility ArchiveVisibility
    | ToggleSourceVisibility Source
    | ToggleOnlyStarredVisible
