module Nym exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import Generate
import Html exposing (Html)
import Length
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
import Triangle3d exposing (Triangle3d)
import TupleHelpers
import Types exposing (..)
import Utils exposing (..)
import Vector3 exposing (Vector3)
import Vector3d


makeNymEntity : NymTemplate -> Scene3d.Entity ()
makeNymEntity nymTemplate =
    let
        allFeatures =
            Scene3d.group
                [ middleGroup
                , symmetryGroup
                , copiedSymmetryGroup
                , testEntity
                ]

        middleGroup : Scene3d.Entity ()
        middleGroup =
            Scene3d.group
                [ crownFace
                , noseBridgeFace
                , noseFrontFace
                , chinFrontFace
                , chinBottomFace
                ]

        crownFace : Scene3d.Entity ()
        crownFace =
            Result.map2
                (\crown brow ->
                    meterQuad
                        (defaultAndLogColorError "crown" nymTemplate.coloring.crown)
                        brow
                        crown
                        (crown |> mirrorPoint)
                        (brow |> mirrorPoint)
                )
                nymTemplate.baseStructure.crown
                nymTemplate.baseStructure.brow
                |> defaultAndLogEntityError "crown"

        noseBridgeFace : Scene3d.Entity ()
        noseBridgeFace =
            Result.map2
                (\brow noseTip ->
                    meterQuad
                        (defaultAndLogColorError "noseBridge" nymTemplate.coloring.bridge)
                        noseTip
                        brow
                        (brow |> mirrorPoint)
                        (noseTip |> mirrorPoint)
                )
                nymTemplate.baseStructure.brow
                nymTemplate.baseStructure.noseTip
                |> defaultAndLogEntityError "noseBridge"

        noseFrontFace : Scene3d.Entity ()
        noseFrontFace =
            Result.map2
                (\mouthCorner noseTip ->
                    meterQuad
                        (defaultAndLogColorError "noseFront" nymTemplate.coloring.noseTip)
                        mouthCorner
                        noseTip
                        (noseTip |> mirrorPoint)
                        (mouthCorner |> mirrorPoint)
                )
                nymTemplate.baseStructure.mouthCorner
                nymTemplate.baseStructure.noseTip
                |> defaultAndLogEntityError "noseFront"

        chinFrontFace : Scene3d.Entity ()
        chinFrontFace =
            Result.map2
                (\mouthCorner chinBottom ->
                    meterQuad
                        (defaultAndLogColorError "chinFront" nymTemplate.coloring.chinFront)
                        mouthCorner
                        chinBottom
                        (chinBottom |> mirrorPoint)
                        (mouthCorner |> mirrorPoint)
                )
                nymTemplate.baseStructure.mouthCorner
                nymTemplate.baseStructure.chinBottom
                |> defaultAndLogEntityError "chinFront"

        chinBottomFace : Scene3d.Entity ()
        chinBottomFace =
            Result.map2
                (\jawBottom chinBottom ->
                    meterQuad
                        (defaultAndLogColorError "chinFront" nymTemplate.coloring.chinBottom)
                        chinBottom
                        jawBottom
                        (jawBottom |> mirrorPoint)
                        (chinBottom |> mirrorPoint)
                )
                nymTemplate.baseStructure.jawBottom
                nymTemplate.baseStructure.chinBottom
                |> defaultAndLogEntityError "chinFront"

        symmetryGroup =
            Scene3d.group
                [ upperTempleFace
                , lowerTempleFace
                , cheekFace
                , upperJawSideFace
                , lowerJawSideFace
                ]

        upperTempleFace : Scene3d.Entity ()
        upperTempleFace =
            Result.map3
                (\brow outerTop crown ->
                    meterTriangle
                        (defaultAndLogColorError "templeFace" nymTemplate.coloring.upperTemple)
                        brow
                        outerTop
                        crown
                )
                nymTemplate.baseStructure.brow
                nymTemplate.baseStructure.outerTop
                nymTemplate.baseStructure.crown
                |> defaultAndLogEntityError "templeFace"

        lowerTempleFace : Scene3d.Entity ()
        lowerTempleFace =
            Result.map3
                (\jawBottom outerTop brow ->
                    meterTriangle
                        (defaultAndLogColorError "templeFace" nymTemplate.coloring.lowerTemple)
                        jawBottom
                        outerTop
                        brow
                )
                nymTemplate.baseStructure.jawBottom
                nymTemplate.baseStructure.outerTop
                nymTemplate.baseStructure.brow
                |> defaultAndLogEntityError "templeFace"

        cheekFace : Scene3d.Entity ()
        cheekFace =
            Result.map3
                (\jawBottom brow noseTip ->
                    meterTriangle
                        (defaultAndLogColorError "cheekFace" nymTemplate.coloring.cheek)
                        jawBottom
                        brow
                        noseTip
                )
                nymTemplate.baseStructure.jawBottom
                nymTemplate.baseStructure.brow
                nymTemplate.baseStructure.noseTip
                |> defaultAndLogEntityError "cheekFace"

        upperJawSideFace : Scene3d.Entity ()
        upperJawSideFace =
            Result.map3
                (\jawBottom noseTip mouthCorner ->
                    meterTriangle
                        (defaultAndLogColorError "upperJawSideFace" nymTemplate.coloring.upperJawSide)
                        jawBottom
                        noseTip
                        mouthCorner
                )
                nymTemplate.baseStructure.jawBottom
                nymTemplate.baseStructure.noseTip
                nymTemplate.baseStructure.mouthCorner
                |> defaultAndLogEntityError "upperJawSideFace"

        lowerJawSideFace : Scene3d.Entity ()
        lowerJawSideFace =
            Result.map3
                (\jawBottom mouthCorner chinBottom ->
                    meterTriangle
                        (defaultAndLogColorError "lowerJawSideFace" nymTemplate.coloring.lowerJawSide)
                        jawBottom
                        mouthCorner
                        chinBottom
                )
                nymTemplate.baseStructure.jawBottom
                nymTemplate.baseStructure.mouthCorner
                nymTemplate.baseStructure.chinBottom
                |> defaultAndLogEntityError "lowerJawSideFace"

        copiedSymmetryGroup =
            symmetryGroup
                |> mirrorGroup

        -- todo: add back in symmetry
        testEntity =
            Scene3d.nothing

        -- Scene3d.point
        --     { radius = Pixels.pixels 10 }
        --     (Material.color Color.black)
        --     (nymTemplate.coreStructure.point
        --         |> Result.withDefault Vector3.zero
        --         |> Vector3.toMetersPoint
        --     )
    in
    allFeatures


meterQuad : Material.Textured () -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> Scene3d.Entity ()
meterQuad material v1 v2 v3 v4 =
    Scene3d.quad
        material
        (v1 |> Vector3.toMetersPoint)
        (v2 |> Vector3.toMetersPoint)
        (v3 |> Vector3.toMetersPoint)
        (v4 |> Vector3.toMetersPoint)


meterTriangle : Material.Plain () -> Vector3 -> Vector3 -> Vector3 -> Scene3d.Entity ()
meterTriangle material v1 v2 v3 =
    Scene3d.triangle
        material
    <|
        Triangle3d.fromVertices
            ( v1 |> Vector3.toMetersPoint
            , v2 |> Vector3.toMetersPoint
            , v3 |> Vector3.toMetersPoint
            )


binarySourceToNym : Bool -> BinarySource -> ( Int, NymTemplate )
binarySourceToNym defaultErrors source =
    let
        ( rSource1, coreStructureTemplate ) =
            Generate.consumeCoreStructureToTemplate source

        ( rSource2, eyeTemplate ) =
            Generate.consumeEyeToTemplate rSource1

        ( rSource3, coloringTemplate ) =
            Generate.consumeColoringToTemplate rSource2
    in
    ( BinarySource.remainingBits rSource3
    , NymTemplate
        coreStructureTemplate
        eyeTemplate
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


defaultAndLogColorError : String -> Result GenError Color -> Material.Material coordinates attributes
defaultAndLogColorError name =
    Result.Extra.unpack
        (\err ->
            let
                _ =
                    Debug.log ("Color " ++ name ++ " failed") err
            in
            Material.color Color.black
        )
        Material.color


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
                , bridge = coloring.bridge |> Result.withDefault Color.red |> Ok
                , noseTip = coloring.noseTip |> Result.withDefault Color.darkRed |> Ok
                , chinFront = coloring.chinFront |> Result.withDefault Color.lightOrange |> Ok
                , chinBottom = coloring.chinBottom |> Result.withDefault Color.orange |> Ok
                , upperTemple = coloring.upperTemple |> Result.withDefault Color.darkOrange |> Ok
                , lowerTemple = coloring.lowerTemple |> Result.withDefault Color.lightYellow |> Ok
                , cheek = coloring.cheek |> Result.withDefault Color.yellow |> Ok
                , upperJawSide = coloring.upperJawSide |> Result.withDefault Color.darkYellow |> Ok
                , lowerJawSide = coloring.lowerJawSide |> Result.withDefault Color.lightGreen |> Ok
            }
        , baseStructure =
            let
                coreStructure =
                    template.baseStructure
            in
            { coreStructure
                | crown = coreStructure.crown |> Result.withDefault (Vector3 0.5 1 0) |> Ok
                , outerTop = coreStructure.outerTop |> Result.withDefault (Vector3 1 0.5 0) |> Ok
                , jawBottom = coreStructure.jawBottom |> Result.withDefault (Vector3 1 -1 0) |> Ok
                , chinBottom = coreStructure.chinBottom |> Result.withDefault (Vector3 0.2 -1 0.9) |> Ok
                , mouthCorner = coreStructure.mouthCorner |> Result.withDefault (Vector3 0.23 -0.9 1) |> Ok
                , noseTip = coreStructure.noseTip |> Result.withDefault (Vector3 0.18 -0.8 1) |> Ok
                , brow = coreStructure.brow |> Result.withDefault (Vector3 0.3 0.4 0.3) |> Ok
            }
    }
