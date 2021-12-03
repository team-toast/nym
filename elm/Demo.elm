module Demo exposing (main)

import Angle
import Axis3d
import BinarySource exposing (BinarySource)
import Browser
import Browser.Events
import Camera3d
import Color
import Crypto.Hash as Hash
import Direction3d
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Generate
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Length
import LineSegment3d exposing (LineSegment3d)
import List.Extra
import Maybe.Extra
import Mouse
import Nym exposing (..)
import Pixels
import Point2d exposing (Point2d, xCoordinate, yCoordinate)
import Point3d
import Random
import Scene3d
import Scene3d.Material as Material exposing (Material)
import TupleHelpers
import Types exposing (..)
import Vector3 exposing (Vector3)
import Vector3d
import Viewpoint3d


showDebugLines =
    False


type Msg
    = MouseMove Mouse.MoveData
    | NewSeed Int
    | TweakSource TweakSourceCmd
    | ChangeTweakPos Int
    | TweakCopy Int


type alias MouseInput =
    { x : Float
    , y : Float
    }


type alias Model =
    { mouseInput : MouseInput
    , nymEntitiesAndPositions : List ( Scene3d.Entity (), Point3dM )
    , seed : Int
    , tweakPos : Int
    , tweakSource : String
    , numBitsUsed : Int
    }


initModel : Model
initModel =
    let
        firstSeed =
            badHashFunction "1"

        firstTweakSource =
            String.repeat 256 "0"

        ( nymEntitiesAndPositions, numBitsUsed ) =
            genNyms firstTweakSource firstSeed
    in
    { mouseInput = MouseInput 0 0
    , nymEntitiesAndPositions = nymEntitiesAndPositions
    , seed = firstSeed
    , tweakPos = 0
    , tweakSource = firstTweakSource
    , numBitsUsed = numBitsUsed
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
              }
                |> regenerateNymEntitiesAndPositions
            , Cmd.none
            )

        TweakSource sourceTweakCmd ->
            case sourceTweakCmd of
                CursorBack ->
                    ( { model
                        | tweakPos = model.tweakPos - 1 |> wrapTo255
                      }
                    , Cmd.none
                    )

                CursorForward ->
                    ( { model
                        | tweakPos = model.tweakPos + 1 |> wrapTo255
                      }
                    , Cmd.none
                    )

                SetBitTrue ->
                    ( { model
                        | tweakSource =
                            model.tweakSource
                                |> String.toList
                                |> List.Extra.setAt model.tweakPos '1'
                                |> String.fromList
                      }
                        |> regenerateNymEntitiesAndPositions
                    , Cmd.none
                    )

                SetBitFalse ->
                    ( { model
                        | tweakSource =
                            model.tweakSource
                                |> String.toList
                                |> List.Extra.setAt model.tweakPos '0'
                                |> String.fromList
                      }
                        |> regenerateNymEntitiesAndPositions
                    , Cmd.none
                    )

                FlipBit ->
                    ( { model
                        | tweakSource =
                            model.tweakSource
                                |> String.toList
                                |> List.Extra.updateAt model.tweakPos
                                    (\bitChar ->
                                        if bitChar == '0' then
                                            '1'

                                        else
                                            '0'
                                    )
                                |> String.fromList
                      }
                        |> regenerateNymEntitiesAndPositions
                    , Cmd.none
                    )

        TweakCopy i ->
            ( { model
                | tweakSource =
                    demoNymSources model.tweakSource model.seed
                        |> List.Extra.getAt i
                        |> Maybe.map BinarySource.getBitsString
                        |> Maybe.withDefault model.tweakSource
              }
                |> regenerateNymEntitiesAndPositions
            , Cmd.none
            )

        ChangeTweakPos i ->
            ( { model
                | tweakPos = i
              }
            , Cmd.none
            )


regenerateNymEntitiesAndPositions : Model -> Model
regenerateNymEntitiesAndPositions model =
    let
        (nymEntitiesAndPositions, bitsUsed) =
            genNyms model.tweakSource model.seed
    in
    { model
        | nymEntitiesAndPositions = nymEntitiesAndPositions
        , numBitsUsed = bitsUsed
    }


wrapTo255 : Int -> Int
wrapTo255 i =
    if i > 255 then
        i - 256

    else if i < 0 then
        i + 256

    else
        i


demoBinarySourceLength : Int
demoBinarySourceLength =
    256


demoNymSources : String -> Int -> List BinarySource
demoNymSources tweakSourceStr seed =
    ([ tweakSourceStr
     , "111111111111111111111111"
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
    List.Extra.initialize 14 initFunc


remainingBitsAndDemoNymTemplates : String -> Int -> List ( String, Int, NymTemplate )
remainingBitsAndDemoNymTemplates tweakSourceStr seed =
    demoNymSources tweakSourceStr seed
        |> List.map binarySourceToNym


genNyms : String -> Int -> ( List ( Scene3d.Entity (), Point3dM ), Int )
genNyms tweakSourceStr seed =
    genNymEntitiesBitsUsedAndPositions tweakSourceStr seed
        |> Tuple.mapFirst
            (List.indexedMap
                (\i ( _, entity, point ) ->
                    ( entity, point )
                )
            )


genNymEntitiesBitsUsedAndPositions : String -> Int -> ( List ( String, Scene3d.Entity (), Point3dM ), Int )
genNymEntitiesBitsUsedAndPositions tweakSourceStr seed =
    let
        bitsLeftAndTemplates =
            remainingBitsAndDemoNymTemplates tweakSourceStr seed

        bitsUsed =
            bitsLeftAndTemplates
                |> List.head
                |> Maybe.map TupleHelpers.tuple3Middle
                |> Maybe.map
                    (\bitsLeft ->
                        256 - bitsLeft
                    )
                |> Maybe.withDefault 0

        _ =
            Debug.log "bits used"
                ((toFloat bitsUsed / 256)
                    * 100
                    |> floor
                    |> String.fromInt
                    |> (\p -> p ++ "% (" ++ String.fromInt bitsUsed ++ ")")
                )
    in
    ( bitsLeftAndTemplates
        |> List.indexedMap
            (\i ( usedBitsString, _, nymTemplate ) ->
                let
                    nymPosition =
                        let
                            xFactor =
                                ((i |> modBy 4 |> toFloat) / 3.0) - 0.5

                            yFactor =
                                ((i // 4 |> toFloat) / 3.0) - 0.5

                            ( x, y ) =
                                ( xFactor * 8
                                , yFactor * -8
                                )
                        in
                        Point3d.meters x y 0
                in
                ( usedBitsString, Nym.makeNymEntity showDebugLines nymTemplate, nymPosition )
            )
    , bitsUsed
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
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 10
            ]
            [ viewTweakUX model.tweakSource model.tweakPos model.numBitsUsed
            , viewNyms model.mouseInput model.nymEntitiesAndPositions
            ]


viewTweakUX : String -> Int -> Int -> Element Msg
viewTweakUX tweakSource tweakPos lastBitUsed =
    Element.row
        [ Element.centerX
        , Element.padding 10
        , Element.spacing 10
        ]
        [ viewTweakPos tweakPos
        , viewTweakSource tweakSource tweakPos lastBitUsed
        , viewTweakCopyUX
        ]


viewTweakPos : Int -> Element Msg
viewTweakPos tweakPos =
    Element.el
        [ Element.centerY
        , Font.size 20
        , Element.width <| Element.px 40
        ]
    <|
        Element.el
            [ Element.alignRight
            ]
            (Element.text <| String.fromInt tweakPos)


viewTweakSource : String -> Int -> Int -> Element Msg
viewTweakSource tweakSource tweakPos lastBitUsed =
    Element.column
        [ Font.size 10
        ]
        (tweakSource
            |> String.toList
            |> List.indexedMap
                (\i bitChar ->
                    let
                        attributes =
                            [ Border.width 1 ]
                                ++ (if i == tweakPos then
                                        [ Border.color (Element.rgb 1 0 0)
                                        ]

                                    else
                                        [ Border.color (Element.rgb 1 1 1)
                                        , Element.pointer
                                        , Events.onClick (ChangeTweakPos i)
                                        ]
                                   )
                                ++ (if i > lastBitUsed then
                                        [ Font.color <| Element.rgb 0.7 0.7 0.7 ]

                                    else
                                        []
                                   )
                    in
                    Element.el attributes
                        (Element.text (String.fromChar bitChar))
                )
            |> List.Extra.greedyGroupsOf 64
            |> List.map
                (Element.row
                    []
                )
        )


viewTweakCopyUX : Element Msg
viewTweakCopyUX =
    let
        buttonElements =
            List.range 0 15
                |> List.map
                    (\i ->
                        Element.el
                            [ Element.height <| Element.px 10
                            , Element.width <| Element.px 10
                            ]
                            (if i == 0 then
                                Element.none

                             else
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    , Element.padding 2
                                    , Element.pointer
                                    , Events.onClick <| TweakCopy i
                                    , Border.width 1
                                    , Border.color <| Element.rgb 0 0 1
                                    ]
                                    Element.none
                            )
                    )
    in
    buttonElements
        |> List.Extra.greedyGroupsOf 4
        |> List.map (Element.row [ Element.spacing 2 ])
        |> Element.column [ Element.spacing 2 ]


viewNyms : MouseInput -> List ( Scene3d.Entity (), Point3dM ) -> Element Msg
viewNyms mouseInput nymEntitiesAndPositions =
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
                            nymEntitiesAndPositions
                                |> rotateNyms mouseInput
                        , camera =
                            Camera3d.perspective
                                { viewpoint =
                                    Viewpoint3d.lookAt
                                        { focalPoint = Point3d.origin
                                        , eyePoint =
                                            Point3d.meters 0 0 20
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
                    -- |> Scene3d.rotateAround Axis3d.y (Angle.degrees 90)
                    |> Scene3d.translateBy
                        (Vector3d.from Point3d.origin position)
            )


subscriptions : Model -> Sub.Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder
        |> Sub.map
            (\keyString ->
                case interpretTweakSourceCmd keyString of
                    Just cmd ->
                        TweakSource cmd

                    Nothing ->
                        NewSeed <| badHashFunction <| keyString
            )


type TweakSourceCmd
    = CursorForward
    | CursorBack
    | SetBitTrue
    | SetBitFalse
    | FlipBit


interpretTweakSourceCmd : String -> Maybe TweakSourceCmd
interpretTweakSourceCmd s =
    if s == "ArrowRight" then
        Just CursorForward

    else if s == "ArrowLeft" then
        Just CursorBack

    else if s == "ArrowUp" then
        Just SetBitTrue

    else if s == "ArrowDown" then
        Just SetBitFalse

    else if s == " " then
        Just FlipBit

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
