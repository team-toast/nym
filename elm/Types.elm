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


type alias CoreStructureTemplate =
    { point : Result GenError Vector3
    }


type alias ColoringTemplate =
    { color : Result GenError Color
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
    { coreStructure : CoreStructureTemplate
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
