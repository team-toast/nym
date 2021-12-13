module Demos.Landing.State exposing (..)

import Demos.Common
import Demos.Landing.Types exposing (..)


init : Flags -> ( Model, Cmd Msg )
init initialSeed =
    ( { morphingModel =
            initMorphingModel initialSeed
      }
    , Cmd.none
    )


initMorphingModel : Int -> MorphingModel
initMorphingModel seed =
    let
        initialNym =
            Demos.Common.genNymTemplate seed
    in
    { oldNym = initialNym
    , newNym = initialNym
    , morphProgress = 1
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
