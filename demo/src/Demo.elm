module Demo exposing (main)

import Angle
import Axis3d
import BinarySource exposing (BinarySource)
import Browser
import Camera3d
import Color
import Direction3d
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Eth.Types exposing (Address)
import Eth.Utils
import Generate
import Hex
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Length
import List.Extra
import Maybe.Extra
import Mouse
import Nym exposing (..)
import Pixels
import Point2d exposing (Point2d, xCoordinate, yCoordinate)
import Point3d
import Quantity
import Random
import Scene3d
import Scene3d.Material as Material
import Svg
import Svg.Attributes as SvgA
import Types exposing (..)
import Vector3d
import Viewpoint3d


demoNymSources : Int -> List BinarySource
demoNymSources seed =
    (List.map BinarySource.fromBitsString
        [ "111111111111111111111111"
        , "000000000000000000000000"
        , "101010101010101010101010"
        , "010101010101010101010101"
        ]
        |> Maybe.Extra.values
    )
        ++ randomBinarySources seed
        ++ (List.map BinarySource.fromBitsString
                [ "111111111111000000000000"
                , "000000000000111111111111"
                , "000000111111000000111111"
                , "111111000000111111000000"
                ]
                |> Maybe.Extra.values
           )


randomSourceLength : Int
randomSourceLength =
    24


randomBinarySources : Int -> List BinarySource
randomBinarySources masterSeed =
    let
        bitGenerator : Random.Generator Char
        bitGenerator =
            Random.uniform '0' [ '1' ]

        initFunc : Int -> BinarySource
        initFunc partialSeed =
            let
                initialSeed =
                    Random.initialSeed <| masterSeed + partialSeed

                unfoldFunc : ( Int, Random.Seed ) -> Maybe ( Char, ( Int, Random.Seed ) )
                unfoldFunc ( count, seed ) =
                    if count < randomSourceLength then
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
    in
    List.Extra.initialize 8 initFunc


demoNyms : Int -> List (Result (List (List Generate.GenError)) Nym)
demoNyms seed =
    demoNymSources seed
        |> List.map binarySourceToNym


type Msg
    = MouseMove Mouse.MoveData


type alias MouseInput =
    { x : Float
    , y : Float
    }


type alias Model =
    { mouseInput : MouseInput
    , seed : Int
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { mouseInput = MouseInput 0 0
            , seed = 0
            }
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseMove moveData ->
            { model
                | mouseInput =
                    MouseInput
                        (toFloat moveData.offsetX / moveData.offsetWidth - 0.5)
                        (toFloat moveData.offsetY / moveData.offsetHeight - 0.5)
            }


genNymEntities : Int -> Point3dM -> List (Scene3d.Entity ())
genNymEntities seed focusPoint =
    demoNyms seed
        |> List.indexedMap
            (\i nymGenResult ->
                case nymGenResult of
                    Err errsLists ->
                        let
                            _ =
                                Debug.log "errors!" errsLists
                        in
                        Scene3d.nothing

                    Ok nym ->
                        let
                            nymPosition =
                                let
                                    xFactor =
                                        ((i |> modBy 4 |> toFloat) / 3.0) - 0.5

                                    yFactor =
                                        ((i // 4 |> toFloat) / 3.0) - 0.5

                                    ( x, y ) =
                                        ( xFactor * 6
                                        , yFactor * -6
                                        )
                                in
                                Point3d.meters x y 0

                            lookDir =
                                Direction3d.from
                                    nymPosition
                                    focusPoint
                                    |> Maybe.withDefault Direction3d.z

                            xAngle =
                                Angle.asin <| Direction3d.xComponent lookDir

                            yAngle =
                                Angle.asin <| -(Direction3d.yComponent lookDir)
                        in
                        Nym.makeNymEntity nym
                            |> Scene3d.rotateAround
                                Axis3d.y
                                xAngle
                            |> Scene3d.rotateAround
                                (Axis3d.x |> Axis3d.rotateAround Axis3d.y xAngle)
                                yAngle
                            |> Scene3d.translateBy
                                (Vector3d.from Point3d.origin nymPosition)
            )


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
        Element.el
            [ Element.centerX
            , Element.centerY
            , Background.color <| Element.rgb 0.9 0.9 1
            , Border.width 1
            , Border.color <| Element.rgb 0.7 0.7 1
            ]
        <|
            Element.html <|
                Html.div
                    [ Html.Events.on
                        "mousemove"
                        (Decode.map MouseMove Mouse.moveDecoder)
                    ]
                <|
                    List.singleton <|
                        Scene3d.unlit
                            { entities =
                                genNymEntities model.seed (mouseInputToNymFocusPoint3d model.mouseInput)

                            -- Provide the camera to be used when rendering the scene
                            , camera =
                                Camera3d.perspective
                                    { viewpoint =
                                        Viewpoint3d.lookAt
                                            { focalPoint = Point3d.origin
                                            , eyePoint =
                                                Point3d.meters 0 0 16
                                            , upDirection = Direction3d.positiveY
                                            }
                                    , verticalFieldOfView = Angle.degrees 30
                                    }

                            -- Anything closer than 1 meter to the camera will be clipped away
                            -- (this is necessary because of the internals of how WebGL works)
                            , clipDepth = Length.meters 1

                            -- Using a transparent background means that the HTML underneath the
                            -- scene will show through
                            , background = Scene3d.transparentBackground

                            -- Size in pixels of the generated HTML element
                            , dimensions = ( Pixels.int 1200, Pixels.int 800 )
                            }
