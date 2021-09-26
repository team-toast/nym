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


type alias BaseStructureTemplate =
    { crownBack : Result GenError Vector3
    , crownFront : Result GenError Vector3
    , innerBrow : Result GenError Vector3
    , outerBrow : Result GenError Vector3
    , outerEyeBottom : Result GenError Vector3
    , innerEyeBottom : Result GenError Vector3
    , outerTop : Result GenError Vector3
    , jawBottom : Result GenError Vector3
    , noseYandZ : Result GenError (Float, Float)
    }


type alias ColoringTemplate =
    { crown : Result GenError Color
    , forehead : Result GenError Color
    , bridge : Result GenError Color
    , noseTip : Result GenError Color
    , chinFront : Result GenError Color
    , chinBottom : Result GenError Color
    , upperTemple : Result GenError Color
    , lowerTemple : Result GenError Color
    , cheek : Result GenError Color
    , upperJawSide : Result GenError Color
    , lowerJawSide : Result GenError Color
    }


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
    { baseStructure : BaseStructureTemplate
    , eye : EyeTemplate
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
