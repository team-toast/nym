module Demo exposing (main)

import Angle
import Axis3d
import BinarySource exposing (BinarySource)
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Generate
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
import Random
import Scene3d
import Types exposing (..)
import Vector3 exposing (Vector3)
import Vector3d
import Viewpoint3d


type Msg
    = MouseMove Mouse.MoveData
    | NewSeed Int
    | ToggleDefaultErrors


type alias MouseInput =
    { x : Float
    , y : Float
    }


type alias Model =
    { mouseInput : MouseInput
    , nymEntitiesAndPositions : List ( Scene3d.Entity (), Point3dM )
    , seed : Int
    , defaultErrors : Bool
    }


initModel =
    { mouseInput = MouseInput 0 0
    , nymEntitiesAndPositions = genNymEntitiesAndPositions 0 False
    , seed = 0
    , defaultErrors = False
    }


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( initModel
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove moveData ->
            ( { model
                | mouseInput =
                    MouseInput
                        (toFloat moveData.offsetX / moveData.offsetWidth - 0.5)
                        (toFloat moveData.offsetY / moveData.offsetHeight - 0.5)
              }
            , Cmd.none
            )

        NewSeed newSeed ->
            let
                _ =
                    Debug.log "new seed" newSeed
            in
            ( { model
                | seed = newSeed
                , nymEntitiesAndPositions =
                    genNymEntitiesAndPositions newSeed model.defaultErrors
              }
            , Cmd.none
            )

        ToggleDefaultErrors ->
            ( { model
                | defaultErrors = not model.defaultErrors
                , nymEntitiesAndPositions =
                    genNymEntitiesAndPositions model.seed (not model.defaultErrors)
              }
            , Cmd.none
            )


demoBinarySourceLength : Int
demoBinarySourceLength =
    100


demoNymSources : Int -> List BinarySource
demoNymSources seed =
    ([ "111111111111111111111111"
     , "000000000000000000000000"
     , "101010101010101010101010"
     , "010101010101010101010101"
     ]
        |> List.map
            (String.toList
                >> List.Extra.cycle demoBinarySourceLength
                >> String.fromList
            )
        |> List.map BinarySource.fromBitsString
        |> Maybe.Extra.values
    )
        ++ randomBinarySources seed
        ++ ([ "111111111111000000000000"
            , "000000000000111111111111"
            , "000000111111000000111111"
            , "111111000000111111000000"
            ]
                |> List.map
                    (String.toList
                        >> List.Extra.cycle demoBinarySourceLength
                        >> String.fromList
                    )
                |> List.map BinarySource.fromBitsString
                |> Maybe.Extra.values
           )


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
                    if count < demoBinarySourceLength then
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


demoNymTemplates : Int -> Bool -> List NymTemplate
demoNymTemplates seed defaultErrors =
    demoNymSources seed
        |> List.map (binarySourceToNym defaultErrors)


genNymEntitiesAndPositions : Int -> Bool -> List ( Scene3d.Entity (), Point3dM )
genNymEntitiesAndPositions seed defaultErrors =
    let
        errorsAndTemplates =
            demoNymTemplates seed defaultErrors
    in
    errorsAndTemplates
        |> List.indexedMap
            (\i nymTemplate ->
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
                in
                ( Nym.makeNymEntity nymTemplate, nymPosition )
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
                                model.nymEntitiesAndPositions
                                    |> rotateNyms model.mouseInput
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
                            , clipDepth = Length.meters 1
                            , background = Scene3d.transparentBackground
                            , dimensions = ( Pixels.int 1200, Pixels.int 800 )
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
                    |> Scene3d.translateBy
                        (Vector3d.from Point3d.origin position)
            )


subscriptions : Model -> Sub.Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder
        |> Sub.map
            (\keyInt ->
                if keyInt == 32 then
                    ToggleDefaultErrors

                else
                    NewSeed keyInt
            )


keyDecoder : Decode.Decoder Int
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map
            (String.toList
                >> List.head
                >> Maybe.map Char.toCode
                >> Maybe.withDefault 0
            )
