module Transforms exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import List
import List.Extra
import Maybe.Extra
import Point3d exposing (Point3d)
import Result.Extra
import TupleHelpers
import Types exposing (..)
import Utils exposing (..)
import Vector3 exposing (Vector3)
import Vector3d


coreStructureTransforms : List (BinarySource -> BaseStructureTemplate -> ( BinarySource, BaseStructureTemplate ))
coreStructureTransforms =
    [ \source template ->
        -- crownBack X
        source
            -- 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
            |> BinarySource.consumeFloatRange 2
                ( 0.2, 0.7 )
            |> tryApplyToTemplate
                (\xResult ->
                    { template
                        | crownBack =
                            Result.map
                                (\x ->
                                    Vector3
                                        x
                                        1
                                        0
                                )
                                xResult
                    }
                )
    , \source template ->
        -- crownFront
        source
            |> BinarySource.consumeVectorFromBounds 2
                ( Vector3 0.2 0.7 0.4
                , Vector3 0.7 1 0.8
                )
            |> tryApplyToTemplate
                (\pointResult ->
                    { template
                        | crownFront = pointResult
                    }
                )
    , \source template ->
        -- brow (forehead)
        source
            |> BinarySource.consume3
                -- x
                ( BinarySource.consumeFloatRange 2
                    ( 0.2, 0.6 )
                  -- YZ angle from -Y (from crownFront)
                , BinarySource.consumeFloatRange 2
                    ( 0, pi / 6 )
                  -- length of angled line (from crownFront)
                , BinarySource.consumeFloatRange 2
                    ( 0.2, 0.8 )
                )
            |> tryApplyToTemplate
                (\valsResult ->
                    { template
                        | brow =
                            Result.map2
                                (\crownFront ( x, angle, length ) ->
                                    Vector3
                                        x
                                        (crownFront.y + (-length * cos angle))
                                        (crownFront.z + (length * sin angle))
                                )
                                template.crownFront
                                valsResult
                    }
                )
    , \source template ->
        -- outerTop X and Y
        source
            |> BinarySource.consume2
                ( BinarySource.consumeFloatRange 2
                    ( 0.7, 1 )
                , BinarySource.consumeFloatRange 2
                    ( 0, 0.7 )
                )
            |> BinarySource.map
                (\( x, y ) ->
                    Vector3 x y 0
                )
            |> tryApplyToTemplate
                (\pointResult ->
                    { template
                        | outerTop = pointResult
                    }
                )
    , \source template ->
        -- jawBottom x and y
        source
            |> BinarySource.consume2
                ( BinarySource.consumeFloatRange 2
                    ( 0.4, 1 )
                , BinarySource.consumeFloatRange 2
                    ( -0.7, -1 )
                )
            |> BinarySource.map
                (\( x, y ) ->
                    Vector3 x y 0
                )
            |> tryApplyToTemplate
                (\pointResult ->
                    { template
                        | jawBottom = pointResult
                    }
                )
    , \source template ->
        -- nose y and z
        source
            |> BinarySource.consume2
                -- y (ratio from brow to jaw)
                ( BinarySource.consumeFloatRange 2
                    ( 0.2, 1 )
                  -- z (from brow)
                , BinarySource.consumeFloatRange 2
                    ( 0, 0.8 )
                )
            |> tryApplyToTemplate
                (\valsResult ->
                    { template
                        | noseYandZ =
                            Result.map3
                                (\brow jawBottom ( yRatio, relZ ) ->
                                    ( (jawBottom.y - brow.y) * yRatio + brow.y
                                    , brow.z + relZ
                                    )
                                )
                                template.brow
                                template.jawBottom
                                valsResult
                    }
                )
    ]


coloringTransforms : List (BinarySource -> ColoringTemplate -> ( BinarySource, ColoringTemplate ))
coloringTransforms =
    []


tryApplyToTemplate :
    (Result GenError val -> template)
    -> Maybe ( BinarySource, val )
    -> ( BinarySource, template )
tryApplyToTemplate func maybeSourceAndVal =
    let
        result =
            maybeSourceAndVal
                |> Maybe.map Tuple.second
                |> Result.fromMaybe NotEnoughSource

        remainingSource =
            maybeSourceAndVal
                |> Maybe.map Tuple.first
                |> Maybe.withDefault BinarySource.empty
    in
    ( remainingSource
    , func result
    )
