module Decoders exposing (starredAlbums)

import Data exposing (StarredAlbums)
import Json.Decode as D exposing (Decoder)
import Set


starredAlbums : Decoder StarredAlbums
starredAlbums =
    D.list D.string
        |> D.map Set.fromList
