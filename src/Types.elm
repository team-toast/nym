module Types exposing (..)

import Color exposing (Color)
import Length
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)

type alias Point3dM = Point3d Length.Meters ()

type alias Structure =
    { innerBrow : Point3dM
    , outerBrow : Point3dM
    , eyecheek : Point3dM
    , eyenose : Point3dM
    , nosetop : Point3dM
    , outerTemple : Point3dM
    , innerTemple : Point3dM
    , earTip  : Point3dM
    , highCheek : Point3dM
    , midCheek : Point3dM
    , lowCheek : Point3dM
    }


type alias Eye =
    Point3dM


type alias Coloring =
    { eyequad : Color
    , noseBridge : Color
    , temple : Color
    , earFront : Color
    , earBack : Color
    , cheek : Color
    }


type alias Nym =
    { structure : Structure
    , eye : Eye
    , coloring : Coloring
    }


type alias BinarySource =
    String
