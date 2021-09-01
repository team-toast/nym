module Types exposing (..)

import Color exposing (Color)
import Length
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)


type alias Point3dM =
    Point3d Length.Meters ()


type alias Structure =
    { innerBrow : Point3dM
    , outerBrow : Point3dM
    , cheekbone : Point3dM
    , eyecheek : Point3dM
    , eyenose : Point3dM
    , noseTop : Point3dM
    , noseMid : Point3dM
    , noseBottom : Point3dM
    , noseBridge : Point3dM
    , outerTemple : Point3dM
    , innerTemple : Point3dM
    , earTip : Point3dM
    , highCheek : Point3dM
    , midCheek : Point3dM
    , lowCheek : Point3dM
    , outerTopSnout : Point3dM
    , outerBottomSnout : Point3dM
    , crown : Point3dM
    }


testStructure : Structure
testStructure =
    { innerBrow = Point3d.meters 0.1 0.2 0.3
    , outerBrow = Point3d.meters 0.5 0.15 0.4
    , cheekbone = Point3d.meters 0.5 -0.2 0.2
    , eyecheek = Point3d.meters 0.4 0 0.3
    , eyenose = Point3d.meters 0.2 0 0.4
    , noseTop = Point3d.meters 0.05 -0.4 1
    , noseMid = Point3d.meters 0.05 -0.5 1
    , noseBottom = Point3d.meters 0.05 -0.55 0.9
    , noseBridge = Point3d.meters 0.15 0.08 0.45
    , innerTemple = Point3d.meters 0.13 0.4 0.3
    , outerTemple = Point3d.meters 0.4 0.4 0.2
    , earTip = Point3d.meters 0.4 0.8 0.2
    , highCheek = Point3d.meters 0.6 0.5 0
    , midCheek = Point3d.meters 0.7 0 0
    , lowCheek = Point3d.meters 0.7 -0.3 0
    , outerTopSnout = Point3d.meters 0.4 -0.2 0.3
    , outerBottomSnout = Point3d.meters 0.4 -0.4 0.3
    , crown = Point3d.meters 0.15 0.6 0
    }


type alias Coloring =
    { eyequad : Color
    , noseBridge : Color
    , noseSide : Color
    , forehead : Color
    , crown : Color
    , temple : Color
    , earFront : Color
    , earBack : Color
    , cheek : Color
    , cheekSpot : Color
    }


allColors =
    [ Color.lightRed
    , Color.red
    , Color.darkRed
    , Color.lightOrange
    , Color.orange
    , Color.darkOrange
    , Color.lightYellow
    , Color.yellow
    , Color.darkYellow
    , Color.lightGreen
    , Color.green
    , Color.darkGreen
    , Color.lightBlue
    , Color.blue
    , Color.darkBlue
    , Color.lightPurple
    , Color.purple
    , Color.darkPurple
    , Color.lightBrown
    , Color.brown
    , Color.darkBrown
    , Color.black
    , Color.white
    , Color.lightGrey
    , Color.grey
    , Color.darkGrey
    , Color.lightGray
    , Color.gray
    , Color.darkGray
    , Color.lightCharcoal
    , Color.charcoal
    , Color.darkCharcoal
    ]


testColoring : Coloring
testColoring =
    { eyequad = Color.darkOrange
    , noseBridge = Color.brown
    , noseSide = Color.lightBrown
    , forehead = Color.orange
    , crown = Color.lightOrange
    , temple = Color.lightOrange
    , earFront = Color.black
    , earBack = Color.lightRed
    , cheek = Color.brown
    , cheekSpot = Color.darkOrange
    }


allBlackColoring : Coloring
allBlackColoring =
    { eyequad = Color.black
    , noseBridge = Color.black
    , noseSide = Color.black
    , forehead = Color.black
    , crown = Color.black
    , temple = Color.black
    , earFront = Color.black
    , earBack = Color.black
    , cheek = Color.black
    , cheekSpot = Color.black
    }


type alias Eye =
    Point3dM


testEye =
    Point3d.meters 0.3 0.05 0.4



-- testNym : Nym
-- testNym =
--     Nym
--         testStructure
--         testEye
--         testColoring


type alias Nym =
    { structure : Structure
    , eye : Eye
    , coloring : Coloring
    }


squashMaybe : String -> a -> Maybe a -> a
squashMaybe warning default maybeVal =
    case maybeVal of
        Just a ->
            a

        Nothing ->
            let
                _ =
                    Debug.log "maybe squashed:" warning
            in
            default
