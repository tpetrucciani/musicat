port module Ports exposing (setLocalStorage)

import Json.Encode


port setLocalStorage : Json.Encode.Value -> Cmd msg
