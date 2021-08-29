module Nym exposing (..)

import Html exposing (Html)
import List.Extra
import Maybe.Extra
import Nym.Types exposing (..)
import Result.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Point3d


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



-- innerBrow
-- outerBrow
-- eyecheek
-- eyenose


consumeEye : BinarySource -> ( Eye, BinarySource )
consumeEye =
    Debug.todo ""


consumeColoring : BinarySource -> ( Coloring, BinarySource )
consumeColoring =
    Debug.todo ""



-- fromHexString : String -> Int -> Int -> Result Error (Html msg)
-- fromHexString src phaceWidth phaceHeight =
--     let
--         stringIsAllHex =
--             src
--                 |> String.toList
--                 |> List.map Features.hexCharToFloat
--                 |> List.all (\f -> f <= 1)
--     in
--     if stringIsAllHex then
--         src
--             |> Features.generatePhaceFromString
--             |> Maybe.map
--                 (svg
--                     [ width <| String.fromInt phaceWidth
--                     , height <| String.fromInt phaceHeight
--                     , viewBox "-100 -100 200 200"
--                     ]
--                 )
--             |> Result.fromMaybe SourceTooSmall
--     else
--         Err NotHexString
-- addressToRelevantString : Address -> String
-- addressToRelevantString =
--     Eth.Utils.addressToString
--         >> String.dropLeft 2
-- {-| Just get the primary color of the Phace. Can be used to make a unique "character color" that matches the Phace.
-- -}
-- faceColorFromAddress : Address -> Result Error ( Float, Float, Float )
-- faceColorFromAddress address =
--     let
--         maybeFeature =
--             Features.consumeFeatureFromString 0
--                 (addressToRelevantString address)
--                 |> Maybe.map Tuple.first
--     in
--     case maybeFeature of
--         Just (Features.FaceColor ( r, g, b )) ->
--             Ok <|
--                 ( Features.hexCharToFloat r
--                 , Features.hexCharToFloat g
--                 , Features.hexCharToFloat b
--                 )
--         _ ->
--             Err SourceTooSmall
