module Demos.Landing.Main exposing (main)

import Browser
import Demos.Landing.State as State
import Demos.Landing.Types exposing (..)
import Demos.Landing.View as View


main : Program () Model Msg
main =
    Browser.element
        { init = always State.init
        , view = View.view
        , update = State.update
        , subscriptions = State.subscriptions
        }
