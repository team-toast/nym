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
        source
            |> BinarySource.consumeFloatRange 2
                ( 0, 0.7 )
            |> tryApplyToTemplate
                (\xResult ->
                    { template
                        | crown =
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
    ]


coloringTransforms : List (BinarySource -> ColoringTemplate -> ( BinarySource, ColoringTemplate ))
coloringTransforms =
    [ \source template ->
        source
            |> BinarySource.consumeColorFromPallette
            |> tryApplyToTemplate
                (\colorResult ->
                    { template
                        | crown =
                            colorResult
                    }
                )
    ]


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
