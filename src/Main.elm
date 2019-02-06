module Main exposing (main)

import Browser
import Model
import Update
import View


main =
    Browser.document
        { init = Model.init
        , update = Update.update
        , subscriptions = always Sub.none
        , view = View.view
        }
