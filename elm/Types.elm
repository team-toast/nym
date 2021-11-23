module Types exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import Length
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)
import Triangle2d exposing (Triangle2d)
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)


type GenError
    = NotEnoughSource
    | InvalidIndex
    | NotYetSet
    | UnexpectedNothing String
    | OtherError String


type alias Point3dM =
    Point3d Length.Meters ()


type alias StructureTemplate =
    { eyeQuadInfo : Result GenError EyeQuadInfo
    , noseTop : Result GenError Vector3
    , noseBridge : Result GenError Vector3
    , noseBottom : Result GenError Vector3
    , cheekbone : Result GenError Vector3
    , crownFront : Result GenError Vector3
    , faceSideMid : Result GenError Vector3

    -- , crownBack : Result GenError Vector3
    -- , crownFront : Result GenError Vector3
    -- , innerBrow : Result GenError Vector3
    -- , outerBrow : Result GenError Vector3
    -- , outerEyeBottom : Result GenError Vector3
    -- , innerEyeBottom : Result GenError Vector3
    -- , outerTop : Result GenError Vector3
    -- , jawBottom : Result GenError Vector3
    -- , noseYandZ : Result GenError ( Float, Float )
    }


type alias EyeQuadInfo =
    { sketchPlane : SketchPlane3d Length.Meters () {}
    , eyeQuad : Vector3.Quad
    , pupil : Pupil
    }


type alias Pupil =
    List ( Vector3, Vector3, Vector3 )


type alias EyeQuadAndPupil2d =
    { pupil : Pupil2d
    , eyeQuad : Vector2.Quad
    }


type alias Pupil2d =
    List ( Vector2, Vector2, Vector2 )


type alias ColoringTemplate =
    { snoutTop : Result GenError Color
    , snoutSideTopMajor : Result GenError Color
    , snoutSideTopMinor : Result GenError Color
    , snoutSideMiddle : Result GenError Color
    , noseTip : Result GenError Color
    , aboveCheekbone : Result GenError Color
    , bridge : Result GenError Color
    , forehead : Result GenError Color
    , aboveEye : Result GenError Color
    , eyeQuad : Result GenError Color
    , belowEar : Result GenError Color
    }



-- { crown : Result GenError Color
-- , forehead : Result GenError Color
-- , bridge : Result GenError Color
-- , noseTip : Result GenError Color
-- , chinFront : Result GenError Color
-- , chinBottom : Result GenError Color
-- , upperTemple : Result GenError Color
-- , lowerTemple : Result GenError Color
-- , cheek : Result GenError Color
-- , upperJawSide : Result GenError Color
-- , lowerJawSide : Result GenError Color
-- }


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
    , coloring : ColoringTemplate
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
