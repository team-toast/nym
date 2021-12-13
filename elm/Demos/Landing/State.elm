module Demos.Landing.State exposing (..)

import Demos.Common
import Demos.Landing.Types exposing (..)
import Demos.Morph


init : Flags -> ( Model, Cmd Msg )
init initialSeed =
    ( { morphModel =
            Demos.Morph.initModel initialSeed
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        MorphMsg morphMsg ->
            let
                ( morphModel, morphCmd ) =
                    Demos.Morph.update morphMsg model.morphModel
            in
            ( { model
                | morphModel = morphModel
              }
            , Cmd.map MorphMsg morphCmd
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Demos.Morph.subscriptions model.morphModel
        |> Sub.map MorphMsg
