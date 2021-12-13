module Demos.Landing.Types exposing (..)

import Types exposing (..)


type alias Flags =
    Int


type alias Model =
    { morphingModel : MorphingModel }


type Msg
    = NoOp


type alias MorphingModel =
    { oldNym : NymTemplate
    , newNym : NymTemplate
    , morphProgress : Float
    }
