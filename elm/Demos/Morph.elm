module Demos.Morph exposing (..)

import BinarySource exposing (BinarySource)
import Browser
import Browser.Events
import Crypto.Hash as Hash
import Demos.Common
import Element exposing (Element)
import Html exposing (Html)
import Json.Decode as Decode
import LineSegment3d exposing (LineSegment3d, endPoint)
import Mouse
import Nym exposing (..)
import Point2d exposing (Point2d)
import Scene3d.Material as Material exposing (Material)
import Time
import TupleHelpers
import Types exposing (..)
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)


type Msg
    = MouseMove Mouse.MoveData
    | NewSeed
    | AnimateDelta Float
    | MaybeChangeLookDir Time.Posix
    | MaybeChangeSeed Time.Posix
    | UpdateNow Time.Posix
    | NoOp


type alias MouseInput =
    { x : Float
    , y : Float
    }


type alias Model =
    { mouseInput : MouseInput
    , laggedMouse : MouseInput
    , oldNymTemplate : NymTemplate
    , newNymTemplate : NymTemplate
    , morphProgress : Float
    , morphAccel : Float
    , seed : Int
    , lastMouseMoveTime : Time.Posix
    , lastMouseClickTime : Time.Posix
    , now : Time.Posix
    }


initModel : Int -> Model
initModel firstSeed =
    let
        nymTemplate =
            Demos.Common.genNymTemplate firstSeed
    in
    { mouseInput = MouseInput 0 0
    , laggedMouse = MouseInput 0 0
    , oldNymTemplate = nymTemplate
    , newNymTemplate = nymTemplate
    , morphProgress = 1
    , morphAccel = 0
    , seed = firstSeed
    , lastMouseMoveTime = Time.millisToPosix 0
    , lastMouseClickTime = Time.millisToPosix 0
    , now = Time.millisToPosix 0
    }


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( initModel <| badHashFunction "1"
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNow time ->
            ( { model | now = time }
            , Cmd.none
            )

        MouseMove moveData ->
            ( { model
                | mouseInput =
                    MouseInput
                        (toFloat moveData.offsetX / moveData.offsetWidth - 0.5)
                        (toFloat moveData.offsetY / moveData.offsetHeight - 0.5)
                , lastMouseMoveTime = model.now
              }
            , Cmd.none
            )

        NewSeed ->
            ( { model
                | lastMouseClickTime = model.now
              }
                |> updateWithNewSeed
            , Cmd.none
            )

        AnimateDelta delta ->
            let
                mouseInterpConstant =
                    if mouseMoveIsIdle model then
                        0.01

                    else
                        0.1

                morphRateConstant =
                    if mouseClickIsIdle model then
                        20000

                    else
                        5000

                morphAccel =
                    model.morphAccel + (delta / morphRateConstant)
            in
            ( { model
                | morphAccel = morphAccel
                , morphProgress =
                    min
                        1
                        (model.morphProgress + morphAccel)
                , laggedMouse =
                    Vector2.interpolate mouseInterpConstant model.laggedMouse model.mouseInput
              }
            , Cmd.none
            )

        MaybeChangeLookDir time ->
            let
                maybeNewInput =
                    if not <| mouseMoveIsIdle model then
                        Nothing

                    else
                        (Time.posixToMillis time + model.seed)
                            |> pseudoRandomSourceInt
                            |> BinarySource.consume2
                                ( BinarySource.consumeBool
                                , BinarySource.consume2
                                    ( BinarySource.consumeFloatRange 3 ( -0.3, 0.3 )
                                    , BinarySource.consumeFloatRange 3 ( -0.2, 0.4 )
                                    )
                                )
                            |> Maybe.map TupleHelpers.tuple3Middle
                            |> Maybe.andThen
                                (\( shouldChange, newDir ) ->
                                    if shouldChange then
                                        Nothing

                                    else
                                        Just <| Vector2.fromTuple newDir
                                )

                newModel =
                    case maybeNewInput of
                        Just newInput ->
                            { model
                                | mouseInput = newInput
                            }

                        Nothing ->
                            model
            in
            ( newModel
            , Cmd.none
            )

        MaybeChangeSeed time ->
            let
                doChangeSeed =
                    if not <| mouseClickIsIdle model then
                        False

                    else
                        (Time.posixToMillis time + model.seed)
                            |> pseudoRandomSourceInt
                            |> BinarySource.consumeInt 2
                            |> Maybe.map TupleHelpers.tuple3Middle
                            |> Maybe.withDefault 0
                            |> (==) 0

                newModel =
                    if doChangeSeed then
                        model |> updateWithNewSeed

                    else
                        model
            in
            ( newModel
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


mouseMoveIsIdle : Model -> Bool
mouseMoveIsIdle model =
    Time.toSecond Time.utc model.now - Time.toSecond Time.utc model.lastMouseMoveTime > 2


mouseClickIsIdle : Model -> Bool
mouseClickIsIdle model =
    Time.toSecond Time.utc model.now - Time.toSecond Time.utc model.lastMouseClickTime > 4


updateWithNewSeed : Model -> Model
updateWithNewSeed model =
    let
        newSeed =
            model.seed |> cycleSeed
    in
    { model
        | seed = newSeed
        , oldNymTemplate =
            Demos.Common.interpolateNymsForRendering
                model.morphProgress
                model.oldNymTemplate
                model.newNymTemplate
        , newNymTemplate = Demos.Common.genNymTemplate newSeed
        , morphProgress = 0
        , morphAccel = 0
    }


cycleSeed : Int -> Int
cycleSeed oldSeed =
    oldSeed
        |> String.fromInt
        |> badHashFunction


pseudoRandomSourceInt : Int -> BinarySource
pseudoRandomSourceInt =
    modBy 777
        >> String.fromInt
        >> badHashFunction
        >> Demos.Common.seedTo256BinarySource


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 10
            ]
            [ Demos.Common.viewNymWithPixelDimensions
                ( 500, 450 )
                ( "100%", "100%" )
                model.laggedMouse
                (Demos.Common.interpolateNymsForRendering
                    model.morphProgress
                    model.oldNymTemplate
                    model.newNymTemplate
                    |> renderNymTemplate False
                )
                (Decode.map MouseMove Mouse.moveDecoder)
            ]


type UserCmd
    = NewRandomSeed


interpetCmd : String -> Maybe UserCmd
interpetCmd s =
    if s == " " then
        Just NewRandomSeed

    else
        Nothing


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


badHashFunction : String -> Int
badHashFunction =
    -- take a string and turn it into an int. Unique strings map to unique ints.
    Hash.sha224
        >> String.toList
        >> List.map Char.toCode
        >> List.map String.fromInt
        >> List.foldl (++) ""
        >> String.toList
        >> List.take 8
        >> String.fromList
        >> String.toInt
        >> Maybe.withDefault 0


subscriptions : Model -> Sub.Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseDown
            (Decode.succeed NewSeed)
        , Browser.Events.onAnimationFrameDelta
            AnimateDelta
        , Time.every 1000
            MaybeChangeLookDir
        , Time.every 1200
            MaybeChangeSeed
        , Time.every 500
            UpdateNow
        ]
