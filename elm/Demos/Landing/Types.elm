module Demos.Landing.Types exposing (..)

import Demos.Morph
import Types exposing (..)
import Vector2 exposing (Vector2)


type alias Flags =
    Int


type alias Model =
    { morphModel : Demos.Morph.Model }


type Msg
    = NoOp
    | MorphMsg Demos.Morph.Msg
