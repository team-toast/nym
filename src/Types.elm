module Types exposing (..)

import Length
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)


type alias Structure =
    { innerBrow : Point3d Length.Meters ()
    , outerBrow : Point3d Length.Meters ()
    , eyecheek : Point3d Length.Meters ()
    , eyenose : Point3d Length.Meters ()
    }


type alias Eye =
    Point3d Length.Meters ()


type alias Coloring =
    Int


type alias Nym =
    { structure : Structure
    , eye : Eye
    , coloring : Coloring
    }


type alias BinarySource =
    String
