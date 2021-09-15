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
                [ testEntity
                ]

        testEntity =
            Scene3d.point
                { radius = Pixels.pixels 10 }
                (Material.color Color.black)
                (nymTemplate.coreStructure.point
                    |> Result.withDefault Vector3.zero
                    |> Vector3.toMetersPoint
                )
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


binarySourceToNym : Bool -> BinarySource -> NymTemplate
binarySourceToNym defaultErrors source =
    let
        ( rSource1, coreStructureTemplate ) =
            Generate.consumeCoreStructureToTemplate source

        ( rSource2, eyeTemplate ) =
            Generate.consumeEyeToTemplate rSource1

        ( rSource3, coloringTemplate ) =
            Generate.consumeColoringToTemplate rSource2
    in
    NymTemplate
        coreStructureTemplate
        eyeTemplate
        coloringTemplate
        |> (if defaultErrors then
                fillTemplateWithDefaults

            else
                identity
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
                | color = template.coloring.color |> Result.withDefault Color.darkOrange |> Ok
            }
        , coreStructure =
            let
                coreStructure =
                    template.coreStructure
            in
            { coreStructure
                | point = template.coreStructure.point |> Result.withDefault (Vector3 0.1 0.2 0.3) |> Ok
            }
    }
