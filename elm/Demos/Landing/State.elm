module Demos.Landing.State exposing (..)

import Demos.Landing.Types exposing (..)


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Debug.todo ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
