module Types exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import Length
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Vector3 exposing (Vector3)


type GenError
    = NotEnoughSource
    | InvalidIndex
    | NotYetSet
    | OtherError String


type alias Point3dM =
    Point3d Length.Meters ()


type alias Structure =
    { crown : Point3dM
    , innerBrow : Point3dM
    , outerBrow : Point3dM
    , noseTop : Point3dM
    , noseMid : Point3dM
    , noseBottom : Point3dM
    , cheekbone : Point3dM
    , eyecheek : Point3dM
    , eyenose : Point3dM
    , noseBridge : Point3dM
    , outerTemple : Point3dM
    , innerTemple : Point3dM
    , earTip : Point3dM
    , highCheek : Point3dM
    , midCheek : Point3dM
    , lowCheek : Point3dM
    , outerTopSnout : Point3dM
    , outerBottomSnout : Point3dM
    }


type alias StructureTemplate =
    { crown : Result GenError Vector3
    , innerBrow : Result GenError Vector3
    , outerBrow : Result GenError Vector3
    , noseTop : Result GenError Vector3
    , noseMid : Result GenError Vector3
    , noseBottom : Result GenError Vector3
    , cheekbone : Result GenError Vector3
    , eyecheek : Result GenError Vector3
    , eyenose : Result GenError Vector3
    , noseBridge : Result GenError Vector3
    , outerTemple : Result GenError Vector3
    , innerTemple : Result GenError Vector3
    , earTip : Result GenError Vector3
    , highCheek : Result GenError Vector3
    , midCheek : Result GenError Vector3
    , lowCheek : Result GenError Vector3
    , outerTopSnout : Result GenError Vector3
    , outerBottomSnout : Result GenError Vector3
    }



-- testStructure : Structure
-- testStructure =
--     { innerBrow = Vector3 0.1 0.2 0.3
--     , outerBrow = Vector3 0.5 0.15 0.4
--     , cheekbone = Vector3 0.5 -0.2 0.2
--     , eyecheek = Vector3 0.4 0 0.3
--     , eyenose = Vector3 0.2 0 0.4
--     , noseTop = Vector3 0.05 -0.4 1
--     , noseMid = Vector3 0.05 -0.5 1
--     , noseBottom = Vector3 0.05 -0.55 0.9
--     , noseBridge = Vector3 0.15 0.08 0.45
--     , innerTemple = Vector3 0.13 0.4 0.3
--     , outerTemple = Vector3 0.4 0.4 0.2
--     , earTip = Vector3 0.4 0.8 0.2
--     , highCheek = Vector3 0.6 0.5 0
--     , midCheek = Vector3 0.7 0 0
--     , lowCheek = Vector3 0.7 -0.3 0
--     , outerTopSnout = Vector3 0.4 -0.2 0.3
--     , outerBottomSnout = Vector3 0.4 -0.4 0.3
--     , crown = Vector3 0.15 0.6 0
--     }


type alias Coloring =
    { eyequad : Color
    , noseBridge : Color
    , noseSide : Color
    , forehead : Color
    , crown : Color
    , temple : Color
    , earFront : Color
    , earBack : Color
    , cheek1 : Color
    , cheek2 : Color
    , cheek3 : Color
    , cheekSpot : Color
    , chin : Color
    }


type alias ColoringTemplate =
    { eyequad : Result GenError Color
    , noseBridge : Result GenError Color
    , noseSide : Result GenError Color
    , forehead : Result GenError Color
    , crown : Result GenError Color
    , temple : Result GenError Color
    , earFront : Result GenError Color
    , earBack : Result GenError Color
    , cheek1 : Result GenError Color
    , cheek2 : Result GenError Color
    , cheek3 : Result GenError Color
    , cheekSpot : Result GenError Color
    , chin : Result GenError Color
    }


-- testColoring : Coloring
-- testColoring =
--     { eyequad = Color.darkOrange
--     , noseBridge = Color.brown
--     , noseSide = Color.lightBrown
--     , forehead = Color.orange
--     , crown = Color.lightOrange
--     , temple = Color.lightOrange
--     , earFront = Color.black
--     , earBack = Color.lightRed
--     , cheek = Color.brown
--     , cheekSpot = Color.darkOrange
--     , chin = Color.white
--     }


-- allBlackColoring : Coloring
-- allBlackColoring =
--     { eyequad = Color.black
--     , noseBridge = Color.black
--     , noseSide = Color.black
--     , forehead = Color.black
--     , crown = Color.black
--     , temple = Color.black
--     , earFront = Color.black
--     , earBack = Color.black
--     , cheek = Color.black
--     , cheekSpot = Color.black
--     , chin = Color.black
--     }


type alias EyeTemplate =
    Result GenError Vector3


type alias Eye =
    Point3dM


testEye : Eye
testEye =
    Point3d.meters 0.3 0.05 0.4



-- testNym : Nym
-- testNym =
--     Nym
--         testStructure
--         testEye
--         testColoring


type alias NymTemplate =
    { structure : StructureTemplate
    , eye : EyeTemplate
    , coloring : ColoringTemplate
    }


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


type alias Transformer templateType =
    templateType -> templateType


type alias TransformerGenResult templateType =
    Result GenError (Transformer templateType)


type alias IndexedTransformGenerator templateType =
    BinarySource -> Int -> ( BinarySource, TransformerGenResult templateType )
