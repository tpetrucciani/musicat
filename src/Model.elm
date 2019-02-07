module Model exposing (Model(..), init)

import Catalogue
import Data
import Http
import Json.Encode
import Messages


type Model
    = Loading Json.Encode.Value
    | Failure String
    | Success Data.State


init : Json.Encode.Value -> ( Model, Cmd Messages.Msg )
init flags =
    ( Loading flags
    , Http.get
        { url = "data/catalogue.json"
        , expect =
            Http.expectJson Messages.GotCatalogue Catalogue.catalogueDecoder
        }
    )
