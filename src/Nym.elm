module Nym exposing (..)

import Color
import Html exposing (Html)
import Length
import List.Extra
import Maybe.Extra
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
        eyeSquare =
            Scene3d.quad (Material.color Color.blue)
                nym.structure.innerBrow
                nym.structure.outerBrow
                nym.structure.eyecheek
                nym.structure.eyenose
    in
    Scene3d.group [ eyeSquare ]


testStructure : Structure
testStructure =
    Structure
        (Point3d.meters 0.1 0.2 0.5)
        (Point3d.meters 0.5 0.15 0.4)
        (Point3d.meters 0.4 0 0.3)
        (Point3d.meters 0.2 0 0.4)


testNym : Nym
testNym =
    Nym
        testStructure
        (Point2d.meters 0 0)
        0


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
