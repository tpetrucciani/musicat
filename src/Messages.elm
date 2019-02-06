module Messages exposing (Msg(..), SetViewMsg(..))

import Catalogue exposing (..)
import Http


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
