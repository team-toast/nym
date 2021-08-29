module Nym exposing (..)

import Color
import Html exposing (Html)
import Length
import List.Extra
import Maybe.Extra
import Pixels
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Result.Extra
import Scene3d
import Scene3d.Material as Material
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


makeNymEntity : Nym -> Scene3d.Entity ()
makeNymEntity nym =
    let
        -- todo: rather build a list here and fold? or maybe do this elsewhere to consume features?
        eyeSquare =
            Scene3d.quad
                (Material.color nym.coloring.eyequad)
                nym.structure.innerBrow
                nym.structure.outerBrow
                nym.structure.eyecheek
                nym.structure.eyenose

        eyePoint : Scene3d.Entity ()
        eyePoint =
            Scene3d.point
                { radius = Pixels.pixels 3 }
                (Material.color Color.black)
                nym.eye

        copySymmetryGroup =
            Scene3d.group
                [ eyeSquare
                , eyePoint
                ]

        copiedSymmetryGroup =
            copySymmetryGroup
                |> Scene3d.mirrorAcross
                    Plane3d.yz
    in
    Scene3d.group
        [ copySymmetryGroup
        , copiedSymmetryGroup
        ]


testStructure : Structure
testStructure =
    Structure
        (Point3d.meters 0.1 0.2 0.5)
        (Point3d.meters 0.5 0.15 0.4)
        (Point3d.meters 0.4 0 0.3)
        (Point3d.meters 0.2 0 0.4)


testColoring : Coloring
testColoring =
    Coloring
        Color.darkOrange


testNym : Nym
testNym =
    Nym
        testStructure
        (Point3d.meters 0.3 0.05 0.5)
        testColoring


binaryStringToNym : BinarySource -> Nym
binaryStringToNym source =
    let
        ( structure, rSource1 ) =
            consumeStructure source

        ( eye, rSource2 ) =
            consumeEye rSource1

        ( coloring, rSource3 ) =
            consumeColoring rSource2
    in
    Nym
        structure
        eye
        coloring


consumeStructure : BinarySource -> ( Structure, BinarySource )
consumeStructure =
    Debug.todo ""


consumeEye : BinarySource -> ( Eye, BinarySource )
consumeEye =
    Debug.todo ""


consumeColoring : BinarySource -> ( Coloring, BinarySource )
consumeColoring =
    Debug.todo ""
