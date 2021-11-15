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


makeNymEntity : NymTemplate -> Scene3d.Entity ()
makeNymEntity nymTemplate =
    let
        noseTip =
            nymTemplate.structure.noseYandZ
                |> Result.map
                    (\( y, z ) ->
                        Vector3 0.18 y z
                    )

        allFeatures =
            Scene3d.group
                [ middleGroup
                , symmetryGroup
                , copiedSymmetryGroup
                ]

        middleGroup : Scene3d.Entity ()
        middleGroup =
            Scene3d.nothing

        -- Scene3d.group
        --     [ crownFace
        --     -- , foreheadFace
        --     -- , noseBridgeFace
        --     , chinBottomFace
        --     ]
        symmetryGroup =
            Scene3d.group
                [ eyeQuadAndPupil

                -- , upperTempleFace
                -- , lowerTempleFace
                -- , cheekFace
                -- , jawSideFace
                -- , testEye
                , testEntity
                ]

        eyeQuadAndPupil : Scene3d.Entity ()
        eyeQuadAndPupil =
            nymTemplate.structure.eyeQuadAndPupil
                |> Result.map
                    (\( eyeQuad, pupil ) ->
                        Scene3d.group
                            [ meterQuad
                                Color.green
                                eyeQuad.bottomRight
                                eyeQuad.bottomLeft
                                eyeQuad.topLeft
                                eyeQuad.topRight
                            , pupil
                                |> List.map
                                    (\triangle ->
                                        meterTriangle
                                            Color.red
                                            (triangle |> TupleHelpers.tuple3First)
                                            (triangle |> TupleHelpers.tuple3Middle)
                                            (triangle |> TupleHelpers.tuple3Last)
                                    )
                                |> Scene3d.group
                            ]
                    )
                |> defaultAndLogEntityError "eyeQuadAndPupil"

        crownFace : Scene3d.Entity ()
        crownFace =
            meterQuadWithDefaults
                "crown"
                nymTemplate.coloring.crown
                nymTemplate.structure.crownFront
                nymTemplate.structure.crownBack
                (nymTemplate.structure.crownBack |> Result.map mirrorPoint)
                (nymTemplate.structure.crownFront |> Result.map mirrorPoint)

        -- foreheadFace : Scene3d.Entity ()
        -- foreheadFace =
        --     meterQuadWithDefaults
        --         "forehead"
        --         nymTemplate.coloring.forehead
        --         nymTemplate.baseStructure.innerBrow
        --         nymTemplate.baseStructure.crownFront
        --         (nymTemplate.baseStructure.crownFront |> Result.map mirrorPoint)
        --         (nymTemplate.baseStructure.innerBrow |> Result.map mirrorPoint)
        -- noseBridgeFace : Scene3d.Entity ()
        -- noseBridgeFace =
        --     meterQuadWithDefaults
        --         "noseBridge"
        --         nymTemplate.coloring.bridge
        --         noseTip
        --         nymTemplate.baseStructure.innerBrow
        --         (nymTemplate.baseStructure.innerBrow |> Result.map mirrorPoint)
        --         (noseTip |> Result.map mirrorPoint)
        chinBottomFace : Scene3d.Entity ()
        chinBottomFace =
            meterQuadWithDefaults
                "chinFront"
                nymTemplate.coloring.chinBottom
                noseTip
                nymTemplate.structure.jawBottom
                (nymTemplate.structure.jawBottom |> Result.map mirrorPoint)
                (noseTip |> Result.map mirrorPoint)

        upperTempleFace : Scene3d.Entity ()
        upperTempleFace =
            meterTriangleWithDefaults
                "upperTempleFace"
                nymTemplate.coloring.upperTemple
                nymTemplate.structure.crownFront
                nymTemplate.structure.outerTop
                nymTemplate.structure.crownBack

        -- lowerTempleFace : Scene3d.Entity ()
        -- lowerTempleFace =
        --     meterTriangleWithDefaults
        --         "lowerTempleFace"
        --         nymTemplate.coloring.upperTemple
        --         nymTemplate.baseStructure.crownFront
        --         nymTemplate.baseStructure.outerTop
        --         nymTemplate.baseStructure.innerBrow
        -- cheekFace : Scene3d.Entity ()
        -- cheekFace =
        --     meterTriangleWithDefaults
        --         "cheekFace"
        --         nymTemplate.coloring.lowerTemple
        --         nymTemplate.baseStructure.jawBottom
        --         nymTemplate.baseStructure.outerTop
        --         nymTemplate.baseStructure.innerBrow
        -- jawSideFace : Scene3d.Entity ()
        -- jawSideFace =
        --     meterTriangleWithDefaults
        --         "jawSideFace"
        --         nymTemplate.coloring.cheek
        --         nymTemplate.baseStructure.jawBottom
        --         nymTemplate.baseStructure.innerBrow
        --         noseTip
        copiedSymmetryGroup =
            symmetryGroup
                |> mirrorGroup

        testEye =
            Scene3d.nothing

        -- nymTemplate.baseStructure.innerBrow
        --     |> Result.map
        --         (\brow ->
        --             meterTriangle (Material.color Color.black)
        --                 brow
        --                 (Vector3.plus brow (Vector3 0.2 -0.1 -0.05))
        --                 (Vector3.plus brow (Vector3 0.2 0.1 -0.05))
        --         )
        --     |> Result.withDefault Scene3d.nothing
        testEntity =
            Scene3d.nothing

        -- Scene3d.group
        --     (List.map
        --         (Scene3d.lineSegment (Material.color Color.blue) << LineSegment3d.fromEndpoints)
        --         [ ( nymTemplate.baseStructure.innerBrow |> Result.withDefault Vector3.zero |> Vector3.toMetersPoint
        --           , nymTemplate.baseStructure.outerBrow |> Result.withDefault Vector3.zero |> Vector3.toMetersPoint
        --           )
        --         , ( nymTemplate.baseStructure.outerBrow |> Result.withDefault Vector3.zero |> Vector3.toMetersPoint
        --           , nymTemplate.baseStructure.outerEyeBottom |> Result.withDefault Vector3.zero |> Vector3.toMetersPoint
        --           )
        --         , ( nymTemplate.baseStructure.outerEyeBottom |> Result.withDefault Vector3.zero |> Vector3.toMetersPoint
        --           , nymTemplate.baseStructure.innerEyeBottom |> Result.withDefault Vector3.zero |> Vector3.toMetersPoint
        --           )
        --         , ( nymTemplate.baseStructure.innerEyeBottom |> Result.withDefault Vector3.zero |> Vector3.toMetersPoint
        --           , nymTemplate.baseStructure.innerBrow |> Result.withDefault Vector3.zero |> Vector3.toMetersPoint
        --           )
        --         ]
        --     )
        -- Scene3d.point
        --     { radius = Pixels.pixels 10 }
        --     (Material.color Color.black)
        --     (nymTemplate.coreStructure.point
        --         |> Result.withDefault Vector3.zero
        --         |> Vector3.toMetersPoint
        --     )
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
                | crown = coloring.crown |> Result.withDefault Color.lightRed |> Ok
                , forehead = coloring.forehead |> Result.withDefault Color.red |> Ok
                , bridge = coloring.bridge |> Result.withDefault Color.darkRed |> Ok
                , noseTip = coloring.noseTip |> Result.withDefault Color.lightOrange |> Ok
                , chinFront = coloring.chinFront |> Result.withDefault Color.orange |> Ok
                , chinBottom = coloring.chinBottom |> Result.withDefault Color.darkOrange |> Ok
                , upperTemple = coloring.upperTemple |> Result.withDefault Color.lightYellow |> Ok
                , lowerTemple = coloring.lowerTemple |> Result.withDefault Color.yellow |> Ok
                , cheek = coloring.cheek |> Result.withDefault Color.darkYellow |> Ok
                , upperJawSide = coloring.upperJawSide |> Result.withDefault Color.lightGreen |> Ok
                , lowerJawSide = coloring.lowerJawSide |> Result.withDefault Color.green |> Ok
            }
        , structure =
            let
                coreStructure =
                    template.structure
            in
            { coreStructure
                | eyeQuadAndPupil = coreStructure.eyeQuadAndPupil |> Result.withDefault defaultEyeQuadAndPupil |> Ok
                , crownBack = coreStructure.crownBack |> Result.withDefault (Vector3 0.5 1 0) |> Ok
                , crownFront = coreStructure.crownFront |> Result.withDefault (Vector3 0.5 1 0.25) |> Ok

                -- , innerBrow = coreStructure.innerBrow |> Result.withDefault (Vector3 0.3 0.4 0.3) |> Ok
                -- , outerBrow = coreStructure.outerBrow |> Result.withDefault (Vector3 0.7 0.45 0.2) |> Ok
                , outerTop = coreStructure.outerTop |> Result.withDefault (Vector3 1 0.5 0) |> Ok
                , jawBottom = coreStructure.jawBottom |> Result.withDefault (Vector3 1 -1 0) |> Ok
                , noseYandZ = coreStructure.noseYandZ |> Result.withDefault ( -0.8, 1 ) |> Ok
            }
    }


defaultEyeQuadAndPupil : ( EyeQuad, Pupil )
defaultEyeQuadAndPupil =
    ( Vector3.Quad
        (Vector3 0 0 0)
        (Vector3 0 0 0)
        (Vector3 0 0 0)
        (Vector3 0 0 0)
    , [ ( Vector3 0 0 0, Vector3 0 0 0, Vector3 0 0 0 ) ]
    )


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
