module MorphDemo exposing (main)

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


showDebugLines =
    False


type Msg
    = MouseMove Mouse.MoveData
    | NewSeed Int
    | AnimateDelta Float
    | MaybeChangeLookDir Time.Posix
    | MaybeChangeSeed Time.Posix
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
    , mouseHasMoved : Bool
    , keyHasPressed : Bool
    }


initModel : Model
initModel =
    let
        firstSeed =
            badHashFunction "1"

        nymTemplate =
            genNymTemplate firstSeed
    in
    { mouseInput = MouseInput 0 0
    , laggedMouse = MouseInput 0 0
    , oldNymTemplate = nymTemplate
    , newNymTemplate = nymTemplate
    , morphProgress = 1
    , morphAccel = 0
    , seed = firstSeed
    , mouseHasMoved = False
    , keyHasPressed = False
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
                , mouseHasMoved = True
              }
            , Cmd.none
            )

        NewSeed newSeed ->
            let
                _ =
                    Debug.log "new seed" newSeed
            in
            ( { model
                | keyHasPressed = True
              }
                |> updateWithNewSeed newSeed
            , Cmd.none
            )

        AnimateDelta delta ->
            let
                mouseInterpConstant =
                    if model.mouseHasMoved then
                        0.1

                    else
                        0.01

                morphAccel =
                    model.morphAccel + (delta / 5000)
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

        -- AnimateTime time ->
        --     ( { model
        --         | mouseInput =
        --             model.mouseInput
        --                 |> (if not model.mouseHasMoved then
        --                         varyMouseInput <| Time.posixToMillis time
        --                     else
        --                         identity
        --                    )
        --       }
        --     , Cmd.none
        --     )
        MaybeChangeLookDir time ->
            let
                maybeNewInput =
                    if model.mouseHasMoved then
                        Nothing

                    else
                        time
                            |> pseudoRandomSourceFromTime
                            |> BinarySource.consume2
                                ( BinarySource.consumeBool
                                , BinarySource.consumeDouble
                                    (BinarySource.consumeFloatRange 3 ( -0.3, 0.3 ))
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
                maybeNewSeed =
                    if model.keyHasPressed then
                        Nothing

                    else
                        time
                            |> pseudoRandomSourceFromTime
                            |> BinarySource.consumeInt 2
                            |> Maybe.map TupleHelpers.tuple3Middle
                            |> Maybe.andThen
                                (\roll ->
                                    if roll == 0 then
                                        Just <| Time.posixToMillis time

                                    else
                                        Nothing
                                )

                newModel =
                    case maybeNewSeed of
                        Just newSeed ->
                            model |> updateWithNewSeed newSeed

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


updateWithNewSeed : Int -> Model -> Model
updateWithNewSeed newSeed model =
    { model
        | seed = newSeed
        , oldNymTemplate =
            interpolateNymsForRendering
                model.morphProgress
                model.oldNymTemplate
                model.newNymTemplate
        , newNymTemplate = genNymTemplate newSeed
        , morphProgress = 0
        , morphAccel = 0
    }


pseudoRandomSourceFromTime : Time.Posix -> BinarySource
pseudoRandomSourceFromTime =
    Time.posixToMillis
        >> String.fromInt
        >> badHashFunction
        >> seedToBinarySource



-- varyMouseInput : Int -> MouseInput -> MouseInput
-- varyMouseInput intSeed mouseInput =
--     Vector2.plus
--         mouseInput
--         (badHashFunction (String.fromInt intSeed)
--             |> seedToBinarySource
--             |> BinarySource.consume2
--                 ( BinarySource.consumeFloatRange 3 ( -0.1, 0.1 )
--                 , BinarySource.consumeFloatRange 3 ( -0.1, 0.1 )
--                 )
--             |> Maybe.map TupleHelpers.tuple3Middle
--             |> Maybe.withDefault ( 0, 0 )
--             |> Vector2.fromTuple
--         )


demoBinarySourceLength : Int
demoBinarySourceLength =
    256


genNymTemplate : Int -> NymTemplate
genNymTemplate seed =
    seed
        |> seedToBinarySource
        |> binarySourceToNym
        |> TupleHelpers.tuple3Last


seedToBinarySource : Int -> BinarySource
seedToBinarySource initialSeedInt =
    let
        bitGenerator : Random.Generator Char
        bitGenerator =
            Random.uniform '0' [ '1' ]

        initialSeed =
            Random.initialSeed initialSeedInt

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
            [ viewNym model.laggedMouse
                (interpolateNymsForRendering
                    model.morphProgress
                    model.oldNymTemplate
                    model.newNymTemplate
                    |> makeNymEntity False
                )
            ]


interpolateNymsForRendering : Float -> NymTemplate -> NymTemplate -> NymTemplate
interpolateNymsForRendering interp start end =
    { structure =
        { eyeQuadInfo =
            Result.map2
                (\startEQInfo endEQInfo ->
                    { sketchPlane =
                        --can ignore, not used in rendering
                        startEQInfo.sketchPlane
                    , eyeQuad =
                        { bottomRight = Vector3.interpolate interp startEQInfo.eyeQuad.bottomRight endEQInfo.eyeQuad.bottomRight
                        , bottomLeft = Vector3.interpolate interp startEQInfo.eyeQuad.bottomLeft endEQInfo.eyeQuad.bottomLeft
                        , topLeft = Vector3.interpolate interp startEQInfo.eyeQuad.topLeft endEQInfo.eyeQuad.topLeft
                        , topRight = Vector3.interpolate interp startEQInfo.eyeQuad.topRight endEQInfo.eyeQuad.topRight
                        }
                    , pupil = interpolatePupil interp startEQInfo.pupil endEQInfo.pupil
                    }
                )
                start.structure.eyeQuadInfo
                end.structure.eyeQuadInfo
        , noseTop =
            Result.map2 (Vector3.interpolate interp) start.structure.noseTop end.structure.noseTop
        , noseBridge =
            Result.map2 (Vector3.interpolate interp) start.structure.noseBridge end.structure.noseBridge
        , noseBottom =
            Result.map2 (Vector3.interpolate interp) start.structure.noseBottom end.structure.noseBottom
        , cheekbone =
            Result.map2 (Vector3.interpolate interp) start.structure.cheekbone end.structure.cheekbone
        , crownFront =
            Result.map2 (Vector3.interpolate interp) start.structure.crownFront end.structure.crownFront
        , crownBack =
            Result.map2 (Vector3.interpolate interp) start.structure.crownBack end.structure.crownBack
        , backZ =
            Result.map2 (interpolateFloat interp) start.structure.backZ end.structure.backZ
        , faceSideTop =
            Result.map2 (Vector3.interpolate interp) start.structure.faceSideTop end.structure.faceSideTop
        , faceSideMid =
            Result.map2 (Vector3.interpolate interp) start.structure.faceSideMid end.structure.faceSideMid
        , faceSideBottom =
            Result.map2 (Vector3.interpolate interp) start.structure.faceSideBottom end.structure.faceSideBottom
        , jawPoint =
            Result.map2 (Vector3.interpolate interp) start.structure.jawPoint end.structure.jawPoint
        , chin =
            Result.map2 (Vector3.interpolate interp) start.structure.chin end.structure.chin
        , earAttachFrontTop =
            Result.map2 (Vector3.interpolate interp) start.structure.earAttachFrontTop end.structure.earAttachFrontTop
        , earAttachFrontBottom =
            Result.map2 (Vector3.interpolate interp) start.structure.earAttachFrontBottom end.structure.earAttachFrontBottom
        , earBaseNormal =
            -- can ignore, not used in rendering
            start.structure.earBaseNormal
        , earAttachBack =
            Result.map2 (Vector3.interpolate interp) start.structure.earAttachBack end.structure.earAttachBack
        , earAttachInside =
            Result.map2 (Vector3.interpolate interp) start.structure.earAttachInside end.structure.earAttachInside
        , earTip =
            Result.map2 (Vector3.interpolate interp) start.structure.earTip end.structure.earTip
        }
    , coloring =
        { snoutTop =
            Result.map2 (Utils.interpolateColors interp) start.coloring.snoutTop end.coloring.snoutTop
        , snoutSideTopMajor =
            Result.map2 (Utils.interpolateColors interp) start.coloring.snoutSideTopMajor end.coloring.snoutSideTopMajor
        , snoutSideTopMinor =
            Result.map2 (Utils.interpolateColors interp) start.coloring.snoutSideTopMinor end.coloring.snoutSideTopMinor
        , snoutSideMiddle =
            Result.map2 (Utils.interpolateColors interp) start.coloring.snoutSideMiddle end.coloring.snoutSideMiddle
        , noseTip =
            Result.map2 (Utils.interpolateColors interp) start.coloring.noseTip end.coloring.noseTip
        , aboveCheekbone =
            Result.map2 (Utils.interpolateColors interp) start.coloring.aboveCheekbone end.coloring.aboveCheekbone
        , bridge =
            Result.map2 (Utils.interpolateColors interp) start.coloring.bridge end.coloring.bridge
        , forehead =
            Result.map2 (Utils.interpolateColors interp) start.coloring.forehead end.coloring.forehead
        , aboveEye =
            Result.map2 (Utils.interpolateColors interp) start.coloring.aboveEye end.coloring.aboveEye
        , eyeQuad =
            Result.map2 (Utils.interpolateColors interp) start.coloring.eyeQuad end.coloring.eyeQuad
        , belowEar =
            Result.map2 (Utils.interpolateColors interp) start.coloring.belowEar end.coloring.belowEar
        , faceSideTop =
            Result.map2 (Utils.interpolateColors interp) start.coloring.faceSideTop end.coloring.faceSideTop
        , faceSideBottom =
            Result.map2 (Utils.interpolateColors interp) start.coloring.faceSideBottom end.coloring.faceSideBottom
        , snoutSideBottom =
            Result.map2 (Utils.interpolateColors interp) start.coloring.snoutSideBottom end.coloring.snoutSideBottom
        , jawSide =
            Result.map2 (Utils.interpolateColors interp) start.coloring.jawSide end.coloring.jawSide
        , mouth =
            Result.map2 (Utils.interpolateColors interp) start.coloring.mouth end.coloring.mouth
        , chinBottom =
            Result.map2 (Utils.interpolateColors interp) start.coloring.chinBottom end.coloring.chinBottom
        , neck =
            Result.map2 (Utils.interpolateColors interp) start.coloring.neck end.coloring.neck
        , crown =
            Result.map2 (Utils.interpolateColors interp) start.coloring.crown end.coloring.crown
        , crownSide =
            Result.map2 (Utils.interpolateColors interp) start.coloring.crownSide end.coloring.crownSide
        , earBackOuter =
            Result.map2 (Utils.interpolateColors interp) start.coloring.earBackOuter end.coloring.earBackOuter
        , earBackInner =
            Result.map2 (Utils.interpolateColors interp) start.coloring.earBackInner end.coloring.earBackInner
        , earFrontOuter =
            Result.map2 (Utils.interpolateColors interp) start.coloring.earFrontOuter end.coloring.earFrontOuter
        , earFrontInner =
            Result.map2 (Utils.interpolateColors interp) start.coloring.earFrontInner end.coloring.earFrontInner
        }
    }


interpolatePupil : Float -> List ( Vector3, Vector3, Vector3 ) -> List ( Vector3, Vector3, Vector3 ) -> List ( Vector3, Vector3, Vector3 )
interpolatePupil interp pupil1 pupil2 =
    if interp == 0 then
        pupil1

    else if interp == 1 then
        pupil2

    else
        let
            ( modifiedPupil1, modifiedPupil2 ) =
                if List.length pupil1 < List.length pupil2 then
                    ( pupil1
                        |> List.Extra.cycle (List.length pupil2)
                    , pupil2
                    )

                else if List.length pupil1 > List.length pupil2 then
                    ( pupil1
                    , pupil2
                        |> List.Extra.cycle (List.length pupil1)
                    )

                else
                    ( pupil1, pupil2 )
        in
        List.Extra.zip
            modifiedPupil1
            modifiedPupil2
            |> List.map
                (\( vectorTupleA, vectorTupleB ) ->
                    TupleHelpers.mergeTuple3
                        ( Vector3.interpolate interp
                        , Vector3.interpolate interp
                        , Vector3.interpolate interp
                        )
                        vectorTupleA
                        vectorTupleB
                )


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat interp f1 f2 =
    (f2 - f1) * interp + f1


viewNym : MouseInput -> Scene3d.Entity () -> Element Msg
viewNym mouseInput interpolatedNym =
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
                        ([ ( interpolatedNym, Point3d.origin ) ]
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


oldViewNym : MouseInput -> Scene3d.Entity () -> Element Msg
oldViewNym mouseInput interpolatedNym =
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
                            [ ( interpolatedNym, Point3d.origin ) ]
                                |> rotateNyms mouseInput
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
        [ Browser.Events.onKeyDown keyDecoder
            |> Sub.map
                (\keyString ->
                    case interpetCmd keyString of
                        Just NewRandomSeed ->
                            NewSeed <| badHashFunction <| String.fromInt <| model.seed

                        Nothing ->
                            NewSeed <| badHashFunction <| keyString
                )
        , Browser.Events.onAnimationFrameDelta
            AnimateDelta
        , Time.every 1000
            MaybeChangeLookDir
        , Time.every 1200
            MaybeChangeSeed
        ]
