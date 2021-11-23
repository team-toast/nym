module Nym exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Generate
import Html exposing (Html)
import Length
import LineSegment3d exposing (LineSegment3d)
import List exposing (range)
import List.Extra
import Maybe.Extra
import Pixels
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Result.Extra
import Scene3d
import Scene3d.Material as Material
import Scene3dHelpers exposing (..)
import SketchPlane3d exposing (SketchPlane3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import TupleHelpers
import Types exposing (..)
import Utils exposing (..)
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)
import Vector3d


makeNymEntity : Bool -> NymTemplate -> Scene3d.Entity ()
makeNymEntity showDebugLines nymTemplate =
    let
        maybeDebugLines =
            if showDebugLines then
                Scene3d.group
                    [ Scene3d.lineSegment
                        (Material.color Color.black)
                        (LineSegment3d.fromEndpoints
                            ( Vector3 0 0 0 |> Vector3.toMetersPoint
                            , Vector3 0 0 2 |> Vector3.toMetersPoint
                            )
                        )
                    ]

            else
                Scene3d.nothing

        allFeatures =
            Scene3d.group <|
                [ middleGroup
                , symmetryGroup
                , copiedSymmetryGroup
                , maybeDebugLines
                ]

        middleGroup : Scene3d.Entity ()
        middleGroup =
            Scene3d.group
                [ snoutTop
                , noseTip
                , bridge
                , forehead
                ]

        symmetryGroup =
            Scene3d.group
                [ eyeQuadAndPupil
                , snoutSideTopMajor
                , snoutSideTopMinor
                , snoutSideMiddle
                , aboveCheekbone
                , aboveEye
                , belowEar
                , testEntity
                ]

        eyeQuadAndPupil : Scene3d.Entity ()
        eyeQuadAndPupil =
            Result.map2
                (\eyeQuadInfo eyeQuadColor ->
                    Scene3d.group
                        [ meterQuad
                            eyeQuadColor
                            eyeQuadInfo.eyeQuad.bottomRight
                            eyeQuadInfo.eyeQuad.bottomLeft
                            eyeQuadInfo.eyeQuad.topLeft
                            eyeQuadInfo.eyeQuad.topRight
                        , eyeQuadInfo.pupil
                            |> List.map
                                (\triangle ->
                                    meterTriangle
                                        Color.black
                                        (triangle |> TupleHelpers.tuple3First)
                                        (triangle |> TupleHelpers.tuple3Middle)
                                        (triangle |> TupleHelpers.tuple3Last)
                                )
                            |> Scene3d.group
                        ]
                )
                nymTemplate.structure.eyeQuadInfo
                nymTemplate.coloring.eyeQuad
                |> defaultAndLogEntityError "eyeQuadAndPupil"

        eyeQuadResult =
            nymTemplate.structure.eyeQuadInfo |> Result.map .eyeQuad

        snoutTop : Scene3d.Entity ()
        snoutTop =
            meterQuadWithDefaults
                "snoutTop"
                nymTemplate.coloring.snoutTop
                nymTemplate.structure.noseTop
                nymTemplate.structure.noseBridge
                (nymTemplate.structure.noseBridge |> Result.map mirrorPoint)
                (nymTemplate.structure.noseTop |> Result.map mirrorPoint)

        snoutSideTopMajor : Scene3d.Entity ()
        snoutSideTopMajor =
            meterQuadWithDefaults
                "snoutSideTopMajor"
                nymTemplate.coloring.snoutSideTopMajor
                (eyeQuadResult |> Result.map .bottomLeft)
                nymTemplate.structure.noseTop
                nymTemplate.structure.cheekbone
                (eyeQuadResult |> Result.map .bottomRight)

        snoutSideTopMinor : Scene3d.Entity ()
        snoutSideTopMinor =
            Scene3d.group
                [ meterTriangleWithDefaults
                    "snoutSideTopMinor"
                    nymTemplate.coloring.snoutSideTopMinor
                    nymTemplate.structure.noseTop
                    nymTemplate.structure.noseBridge
                    (eyeQuadResult |> Result.map .bottomLeft)
                , meterTriangleWithDefaults
                    "snoutSideTopMinor"
                    nymTemplate.coloring.snoutSideTopMinor
                    nymTemplate.structure.noseBridge
                    (eyeQuadResult |> Result.map .topLeft)
                    (eyeQuadResult |> Result.map .bottomLeft)
                ]

        snoutSideMiddle : Scene3d.Entity ()
        snoutSideMiddle =
            meterTriangleWithDefaults
                "snoutSideMiddle"
                nymTemplate.coloring.snoutSideMiddle
                nymTemplate.structure.noseTop
                nymTemplate.structure.cheekbone
                nymTemplate.structure.noseBottom

        noseTip : Scene3d.Entity ()
        noseTip =
            meterQuadWithDefaults
                "noseTip"
                nymTemplate.coloring.noseTip
                nymTemplate.structure.noseBottom
                nymTemplate.structure.noseTop
                (nymTemplate.structure.noseTop |> Result.map mirrorPoint)
                (nymTemplate.structure.noseBottom |> Result.map mirrorPoint)

        aboveCheekbone : Scene3d.Entity ()
        aboveCheekbone =
            meterTriangleWithDefaults
                "aboveCheekbone"
                nymTemplate.coloring.aboveCheekbone
                (eyeQuadResult |> Result.map .bottomRight)
                (eyeQuadResult |> Result.map .topRight)
                nymTemplate.structure.cheekbone

        bridge : Scene3d.Entity ()
        bridge =
            meterQuadWithDefaults
                "bridge"
                nymTemplate.coloring.bridge
                nymTemplate.structure.noseBridge
                (eyeQuadResult |> Result.map .topLeft)
                (eyeQuadResult |> Result.map .topLeft |> Result.map mirrorPoint)
                (nymTemplate.structure.noseBridge |> Result.map mirrorPoint)

        forehead : Scene3d.Entity ()
        forehead =
            meterQuadWithDefaults
                "forehead"
                nymTemplate.coloring.forehead
                (eyeQuadResult |> Result.map .topLeft)
                nymTemplate.structure.crownFront
                (nymTemplate.structure.crownFront |> Result.map mirrorPoint)
                (eyeQuadResult |> Result.map .topLeft |> Result.map mirrorPoint)

        aboveEye : Scene3d.Entity ()
        aboveEye =
            meterTriangleWithDefaults
                "aboveEye"
                nymTemplate.coloring.aboveEye
                (eyeQuadResult |> Result.map .topLeft)
                nymTemplate.structure.crownFront
                (eyeQuadResult |> Result.map .topRight)

        belowEar : Scene3d.Entity ()
        belowEar =
            meterTriangleWithDefaults
                "belowEar"
                nymTemplate.coloring.belowEar
                (eyeQuadResult |> Result.map .topRight)
                nymTemplate.structure.crownFront
                nymTemplate.structure.faceSideMid

        copiedSymmetryGroup =
            symmetryGroup
                |> mirrorGroup

        testEntity =
            Scene3d.nothing
    in
    allFeatures


meterQuad : Color -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> Scene3d.Entity ()
meterQuad color v1 v2 v3 v4 =
    Scene3d.quad
        (Material.color color)
        (v1 |> Vector3.toMetersPoint)
        (v2 |> Vector3.toMetersPoint)
        (v3 |> Vector3.toMetersPoint)
        (v4 |> Vector3.toMetersPoint)


meterQuadWithDefaults :
    String
    -> Result GenError Color
    -> Result GenError Vector3
    -> Result GenError Vector3
    -> Result GenError Vector3
    -> Result GenError Vector3
    -> Scene3d.Entity ()
meterQuadWithDefaults name colorResult v1Result v2Result v3Result v4Result =
    Result.map4
        (\v1 v2 v3 v4 ->
            meterQuad
                (defaultAndLogColorError name colorResult)
                v1
                v2
                v3
                v4
        )
        v1Result
        v2Result
        v3Result
        v4Result
        |> defaultAndLogEntityError name


meterTriangle : Color -> Vector3 -> Vector3 -> Vector3 -> Scene3d.Entity ()
meterTriangle color v1 v2 v3 =
    Scene3d.triangle
        (Material.color color)
    <|
        Triangle3d.fromVertices
            ( v1 |> Vector3.toMetersPoint
            , v2 |> Vector3.toMetersPoint
            , v3 |> Vector3.toMetersPoint
            )


meterTriangleWithDefaults :
    String
    -> Result GenError Color
    -> Result GenError Vector3
    -> Result GenError Vector3
    -> Result GenError Vector3
    -> Scene3d.Entity ()
meterTriangleWithDefaults name colorResult v1Result v2Result v3Result =
    Result.map3
        (\v1 v2 v3 ->
            meterTriangle
                (defaultAndLogColorError name colorResult)
                v1
                v2
                v3
        )
        v1Result
        v2Result
        v3Result
        |> defaultAndLogEntityError name


binarySourceToNym : Bool -> BinarySource -> ( String, Int, NymTemplate )
binarySourceToNym defaultErrors source =
    let
        ( rSource1, coreStructureTemplate ) =
            Generate.consumeCoreStructureToTemplate source

        ( rSource2, eyeTemplate ) =
            Generate.consumeEyeToTemplate rSource1

        ( rSource3, coloringTemplate ) =
            Generate.consumeColoringToTemplate rSource2

        bitsLeft =
            BinarySource.remainingBits rSource3
    in
    ( source
        |> BinarySource.getBitsString
        |> String.dropRight bitsLeft
    , bitsLeft
    , NymTemplate
        coreStructureTemplate
        coloringTemplate
        |> (if defaultErrors then
                fillTemplateWithDefaults

            else
                identity
           )
    )


defaultAndLogEntityError : String -> Result GenError (Scene3d.Entity ()) -> Scene3d.Entity ()
defaultAndLogEntityError name =
    Result.Extra.extract
        (\err ->
            let
                _ =
                    Debug.log ("Entity " ++ name ++ " failed") err
            in
            Scene3d.nothing
        )


defaultAndLogColorError : String -> Result GenError Color -> Color
defaultAndLogColorError name =
    Result.Extra.unpack
        (\err ->
            let
                _ =
                    Debug.log ("Color " ++ name ++ " failed") err
            in
            Color.black
        )
        identity


fillTemplateWithDefaults : NymTemplate -> NymTemplate
fillTemplateWithDefaults template =
    { template
        | coloring =
            let
                coloring =
                    template.coloring
            in
            { coloring
                | snoutTop = coloring.snoutTop |> Result.withDefault Color.lightRed |> Ok
                , snoutSideTopMajor = coloring.snoutSideTopMajor |> Result.withDefault Color.red |> Ok
                , snoutSideTopMinor = coloring.snoutSideTopMinor |> Result.withDefault Color.darkRed |> Ok
                , snoutSideMiddle = coloring.snoutSideMiddle |> Result.withDefault Color.lightOrange |> Ok
                , noseTip = coloring.noseTip |> Result.withDefault Color.orange |> Ok
                , aboveCheekbone = coloring.aboveCheekbone |> Result.withDefault Color.darkOrange |> Ok
                , bridge = coloring.bridge |> Result.withDefault Color.lightYellow |> Ok
                , forehead = coloring.forehead |> Result.withDefault Color.yellow |> Ok
                , aboveEye = coloring.aboveEye |> Result.withDefault Color.darkYellow |> Ok
                , eyeQuad = coloring.eyeQuad |> Result.withDefault Color.lightGreen |> Ok
                , belowEar = coloring.eyeQuad |> Result.withDefault Color.darkGreen |> Ok
            }
        , structure =
            let
                coreStructure =
                    template.structure
            in
            { coreStructure
                | eyeQuadInfo = coreStructure.eyeQuadInfo |> Result.withDefault defaultEyeQuadAndPupil |> Ok
                , noseTop = coreStructure.noseTop |> Result.withDefault (Vector3 0.1 0 1) |> Ok

                -- , crownBack = coreStructure.crownBack |> Result.withDefault (Vector3 0.5 1 0) |> Ok
                -- , crownFront = coreStructure.crownFront |> Result.withDefault (Vector3 0.5 1 0.25) |> Ok
                -- , innerBrow = coreStructure.innerBrow |> Result.withDefault (Vector3 0.3 0.4 0.3) |> Ok
                -- , outerBrow = coreStructure.outerBrow |> Result.withDefault (Vector3 0.7 0.45 0.2) |> Ok
                -- , outerTop = coreStructure.outerTop |> Result.withDefault (Vector3 1 0.5 0) |> Ok
                -- , jawBottom = coreStructure.jawBottom |> Result.withDefault (Vector3 1 -1 0) |> Ok
                -- , noseYandZ = coreStructure.noseYandZ |> Result.withDefault ( -0.8, 1 ) |> Ok
            }
    }


defaultEyeQuadAndPupil : EyeQuadInfo
defaultEyeQuadAndPupil =
    EyeQuadInfo
        SketchPlane3d.xy
        (Vector3.Quad
            (Vector3 0 0 0)
            (Vector3 0 0 0)
            (Vector3 0 0 0)
            (Vector3 0 0 0)
        )
        [ ( Vector3 0 0 0, Vector3 0 0 0, Vector3 0 0 0 ) ]


type alias EyeQuadAndPupil2d =
    { pupil : Pupil2d
    , eyeQuad : EyeQuad2d
    }


type alias Pupil2d =
    List ( Vector2, Vector2, Vector2 )


type alias EyeQuad2d =
    { bottomRight : Vector2
    , bottomLeft : Vector2
    , topLeft : Vector2
    , topRight : Vector2
    }
