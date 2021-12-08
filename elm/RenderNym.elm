module RenderNym exposing (main)

import Angle
import Axis3d
import BinarySource exposing (BinarySource)
import Browser
import Browser.Events
import Camera3d
import Crypto.Hash as Hash
import Direction3d
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Length
import LineSegment3d exposing (LineSegment3d, endPoint)
import List.Extra
import Maybe.Extra
import Mouse
import Nym exposing (..)
import Pixels
import Point2d exposing (Point2d)
import Point3d
import Random
import Scene3d
import Scene3d.Light
import Scene3d.Material as Material exposing (Material)
import Time
import TupleHelpers
import Types exposing (..)
import Utils
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)
import Vector3d
import Viewpoint3d
import WebGL


type Msg
    = MouseMove Mouse.MoveData
    | AnimateDelta Float
    | MaybeChangeLookDir Time.Posix
    | UpdateNow Time.Posix
    | NoOp


type alias MouseInput =
    { x : Float
    , y : Float
    }


type NymRenderError
    = MalformedHexIdentifier
    | NymGenError ( NymTemplate, GenError )


type alias Model =
    { mouseInput : MouseInput
    , laggedMouse : MouseInput
    , nymResult : Result NymRenderError Nym
    , lastMouseMoveTime : Time.Posix
    , now : Time.Posix
    }


type alias Flags =
    String


init : Flags -> ( Model, Cmd Msg )
init nymHexData =
    let
        nymResult =
            BinarySource.fromHexString nymHexData
                |> Result.fromMaybe MalformedHexIdentifier
                |> Result.andThen
                    (binarySourceToNym
                        >> Result.mapError NymGenError
                    )
    in
    ( { mouseInput = MouseInput 0 0
      , laggedMouse = MouseInput 0 0
      , nymResult = nymResult
      , lastMouseMoveTime = Time.millisToPosix 0
      , now = Time.millisToPosix 0
      }
    , Cmd.none
    )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


pseudoRandomSourceFromTime : Time.Posix -> BinarySource
pseudoRandomSourceFromTime =
    Time.posixToMillis
        >> modBy 777
        >> String.fromInt
        >> badHashFunction
        >> seedTo256BinarySource


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

        AnimateDelta delta ->
            let
                mouseInterpConstant =
                    if mouseMoveIsIdle model then
                        0.01

                    else
                        0.1
            in
            ( { model
                | laggedMouse =
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
                        time
                            |> pseudoRandomSourceFromTime
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

        NoOp ->
            ( model
            , Cmd.none
            )


mouseMoveIsIdle : Model -> Bool
mouseMoveIsIdle model =
    Time.toSecond Time.utc model.now - Time.toSecond Time.utc model.lastMouseMoveTime > 2


seedTo256BinarySource : Int -> BinarySource
seedTo256BinarySource initialSeedInt =
    let
        bitGenerator : Random.Generator Char
        bitGenerator =
            Random.uniform '0' [ '1' ]

        initialSeed =
            Random.initialSeed initialSeedInt

        unfoldFunc : ( Int, Random.Seed ) -> Maybe ( Char, ( Int, Random.Seed ) )
        unfoldFunc ( count, seed ) =
            if count < 256 then
                Just <|
                    let
                        ( bit, newSeed ) =
                            Random.step bitGenerator seed
                    in
                    ( bit
                    , ( count + 1
                      , newSeed
                      )
                    )

            else
                Nothing
    in
    List.Extra.unfoldr
        unfoldFunc
        ( 0, initialSeed )
        |> String.fromList
        |> BinarySource.unsafeFromBitsString


mouseInputToNymFocusPoint3d : MouseInput -> Point3dM
mouseInputToNymFocusPoint3d mouseInput =
    Point3d.meters
        (mouseInput.x * 10.4)
        -(mouseInput.y * 10.4)
        2


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        viewNymOrError model.laggedMouse model.nymResult


viewNymOrError : MouseInput -> Result NymRenderError Nym -> Element Msg
viewNymOrError laggedMouse nymResult =
    case nymResult of
        Ok nym ->
            viewNym laggedMouse nym

        Err e ->
            viewRenderError e


viewRenderError : NymRenderError -> Element Msg
viewRenderError err =
    Element.el
        [ Element.centerX
        , Element.centerY
        , Font.size 20
        , Font.color <| Element.rgb 0.3 0.3 0.3
        ]
    <|
        Element.text <|
            case err of
                MalformedHexIdentifier ->
                    "Malformed Nym Identifier - invalid hex uint"

                NymGenError ( template, genErr ) ->
                    let
                        _ =
                            Debug.log "something wrong with this template" template
                    in
                    genErrorToString genErr


viewNym : MouseInput -> Nym -> Element Msg
viewNym mouseInput nym =
    Element.html <|
        Html.div
            [ Html.Events.on
                "mousemove"
                (Decode.map MouseMove Mouse.moveDecoder)
            ]
        <|
            List.singleton <|
                WebGL.toHtml
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "100%"
                    , Html.Attributes.width 500
                    , Html.Attributes.height 450
                    ]
                <|
                    makeWebGLEntities
                        ([ ( renderNym nym, Point3d.origin ) ]
                            |> rotateNyms mouseInput
                        )


makeWebGLEntities : List (Scene3d.Entity ()) -> List WebGL.Entity
makeWebGLEntities nymList =
    Scene3d.toWebGLEntities
        { lights = Scene3d.noLights
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint =
                            Point3d.meters 0 0 5
                        , upDirection = Direction3d.positiveY
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        , clipDepth = Length.meters 1
        , exposure = Scene3d.exposureValue 5
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Scene3d.Light.daylight
        , aspectRatio = 10 / 9
        , supersampling = 1
        , entities = nymList
        }


rotateNyms : MouseInput -> List ( Scene3d.Entity (), Point3dM ) -> List (Scene3d.Entity ())
rotateNyms mouseInput entitiesAndPositions =
    entitiesAndPositions
        |> List.map
            (\( nymEntity, position ) ->
                let
                    focusPoint =
                        mouseInputToNymFocusPoint3d mouseInput

                    lookDir =
                        Direction3d.from
                            position
                            focusPoint
                            |> Maybe.withDefault Direction3d.z

                    xAngle =
                        Angle.asin <| Direction3d.xComponent lookDir

                    yAngle =
                        Angle.asin <| -(Direction3d.yComponent lookDir)
                in
                nymEntity
                    |> Scene3d.rotateAround
                        Axis3d.y
                        xAngle
                    |> Scene3d.rotateAround
                        (Axis3d.x |> Axis3d.rotateAround Axis3d.y xAngle)
                        yAngle
                    -- |> Scene3d.rotateAround Axis3d.y (Angle.degrees 90)
                    |> Scene3d.translateBy
                        (Vector3d.from Point3d.origin position)
            )


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
        [ Browser.Events.onAnimationFrameDelta
            AnimateDelta
        , Time.every 1000
            MaybeChangeLookDir
        , Time.every 500
            UpdateNow
        ]
