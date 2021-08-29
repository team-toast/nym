module Nym exposing (..)

import Html exposing (Html)
import Length
import List.Extra
import Maybe.Extra
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Result.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Structure =
    { innerBrow : Point3d Length.Meters ()
    , outerBrow : Point3d Length.Meters ()
    , eyecheek : Point3d Length.Meters ()
    , eyenose : Point3d Length.Meters ()
    }


type alias Eye =
    Point2d Length.Meters ()


type alias Coloring =
    Int


type alias Nym =
    { structure : Structure
    , eye : Eye
    , coloring : Coloring
    }


type alias BinarySource =
    String


testStructure : Structure
testStructure =
    Structure
        (Point3d.meters 0.1 0.2 0.5)
        (Point3d.meters 0.5 0.15 0.4)
        (Point3d.meters 0.4 0 0.3)
        (Point3d.meters 0.2 0 0.4)


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
