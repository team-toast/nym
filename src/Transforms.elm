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


structureTransforms : List (BinarySource -> StructureTemplate -> ( BinarySource, StructureTemplate ))
structureTransforms =
    [ \source template ->
        source
            |> BinarySource.consumeTwoSimilarValues
                (BinarySource.consumeFloatRange 2 0.1 0.5)
            |> BinarySource.andThenConsume
                (BinarySource.consumeTwoValues
                    --YZ angle from crown to forehead, 0 = forward
                    ( BinarySource.consumeFloatRange 2 -(pi / 8) (pi / 8)
                      -- length of line from crown to forehead
                    , BinarySource.consumeFloatRange 2 0.1 0.4
                    )
                )
                (\( crownX, templeX ) ( yzAngle, crownLength ) ->
                    let
                        crown =
                            Vector3 crownX 0.5 0

                        temple =
                            Vector3
                                templeX
                                (sin yzAngle * crownLength + crown.y)
                                (cos yzAngle * crownLength + crown.z)
                    in
                    ( crown, temple )
                )
            |> tryApplyToTemplate
                (Result.Extra.unpack
                    (\e ->
                        { template
                            | crown = Err e
                            , innerTemple = Err e
                        }
                    )
                    (\( crown, temple ) ->
                        { template
                            | crown = Ok crown
                            , innerTemple = Ok temple
                        }
                    )
                )
    ]


oldStuff =
    [ \source template ->
        -- define crown,temple,brow position
        -- x first, independently
        let
            maxXValue =
                0.3

            minDescendAngle =
                pi / 12

            maxDescendAngle =
                pi / 5

            minVectorLength =
                0.5

            maxVectorLength =
                0.7

            createPoint : ( Float, Float, Float ) -> Vector3
            createPoint ( x, yzAngle, yzLength ) =
                Vector3
                    x
                    (sin yzAngle * yzLength)
                    (cos yzAngle * yzLength)
        in
        source
            |> BinarySource.consumeThreeSimilarValues
                (BinarySource.consumeThreeValues
                    -- x value
                    ( BinarySource.consumeUnsignedFloat 2 maxXValue
                      -- angle in radians in YZ from 'up' relative to previous one
                    , BinarySource.consumeFloatRange 2 minDescendAngle maxDescendAngle
                      -- length
                    , BinarySource.consumeFloatRange 2 minVectorLength maxVectorLength
                    )
                )
            |> BinarySource.map
                (\( crownVals, templeVals, browVals ) ->
                    ( createPoint crownVals
                    , createPoint
                        (templeVals
                            |> TupleHelpers.mapTuple3Middle
                                ((+) (TupleHelpers.tuple3Middle crownVals))
                        )
                    , createPoint
                        (browVals
                            |> TupleHelpers.mapTuple3Middle
                                ((+) (TupleHelpers.tuple3Middle crownVals))
                            |> TupleHelpers.mapTuple3Middle
                                ((+) (TupleHelpers.tuple3Middle templeVals))
                        )
                    )
                )
            |> tryApplyToTemplate
                (\results ->
                    case results of
                        Ok ( crown, temple, brow ) ->
                            { template
                                | crown = Ok crown
                                , innerTemple = Ok temple
                                , innerBrow = Ok brow
                            }

                        Err err ->
                            { template
                                | crown = Err err
                                , innerTemple = Err err
                                , innerBrow = Err err
                            }
                )

    -- , \source template ->
    --     source
    --         |> BinarySource.consumeVectorFromBounds 2
    --             ( Vector3 0 0.4 0
    --             , Vector3 0.5 0.7 0
    --             )
    --         |> tryApplyToTemplate
    --             (\crownResult ->
    --                 { template
    --                     | crown = crownResult
    --                 }
    --             )
    -- , \source template ->
    --     source
    --         |> BinarySource.consumeVectorFromBounds 2
    --             ( Vector3 0.1 -0.1 0.1
    --             , Vector3 0.2 -0.2 0.2
    --             )
    --         |> tryApplyToTemplate
    --             (\templeResult ->
    --                 { template
    --                     | innerTemple =
    --                         Result.map2
    --                             Vector3.plus
    --                             template.crown
    --                             templeResult
    --                 }
    --             )
    -- , \source template ->
    --     source
    --         |> BinarySource.consumeVectorFromBounds 2
    --             ( Vector3 -0.1 -0.1 0
    --             , Vector3 0.1 -0.2 0
    --             )
    --         |> tryApplyToTemplate
    --             (\browResult ->
    --                 { template
    --                     | innerBrow =
    --                         Result.map2
    --                             Vector3.plus
    --                             template.innerTemple
    --                             browResult
    --                 }
    --             )
    ]


testColorTransforms : List (BinarySource -> ColoringTemplate -> ( BinarySource, ColoringTemplate ))
testColorTransforms =
    [ \source template ->
        ( source
        , { template
            | crown = Ok Color.red
            , forehead = Ok Color.green
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
                        | crown = colorResult
                    }
                )
    , \source template ->
        source
            |> BinarySource.consumeVectorDimNeg1to1 1
            |> Maybe.map (Tuple.mapSecond (Vector3.scaleBy 0.1))
            |> tryApplyToTemplate
                (\colorModifierResult ->
                    { template
                        | forehead =
                            Result.map2
                                addVectorToColor
                                colorModifierResult
                                template.crown
                    }
                )
    ]


tryApplyToTemplate : (Result GenError val -> template) -> Maybe ( BinarySource, val ) -> ( BinarySource, template )
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



-- oldStructureTransformGenerators : List (BinarySource -> ( Transformer StructureTemplate, BinarySource ))
-- oldStructureTransformGenerators =
--     [ \source ->
--         ( \template ->
-- { template
--     | innerBrow = Ok <| Vector3 0.1 0.2 0.3
--     , outerBrow = Ok <| Vector3 0.5 0.15 0.4
--     , cheekbone = Ok <| Vector3 0.5 -0.2 0.2
--     , eyecheek = Ok <| Vector3 0.4 0 0.3
--     , eyenose = Ok <| Vector3 0.2 0 0.4
--     , noseTop = Ok <| Vector3 0.05 -0.4 1
--     , noseMid = Ok <| Vector3 0.05 -0.5 1
--     , noseBottom = Ok <| Vector3 0.05 -0.55 0.9
--     , noseBridge = Ok <| Vector3 0.15 0.08 0.45
--     , innerTemple = Ok <| Vector3 0.13 0.4 0.3
--     , outerTemple = Ok <| Vector3 0.4 0.4 0.2
--     , earTip = Ok <| Vector3 0.4 0.8 0.2
--     , highCheek = Ok <| Vector3 0.6 0.5 0
--     , midCheek = Ok <| Vector3 0.7 0 0
--     , lowCheek = Ok <| Vector3 0.7 -0.3 0
--     , outerTopSnout = Ok <| Vector3 0.4 -0.2 0.3
--     , outerBottomSnout = Ok <| Vector3 0.4 -0.4 0.3
--     , crown = Ok <| Vector3 0.15 0.6 0
-- }
--         , source
--         )
--     , \source ->
--         let
--             ( baseVecResult, remainingSource ) =
--                 case BinarySource.consumeVectorDimNeg1to1 2 source of
--                     Just ( uVec, s ) ->
--                         ( Ok uVec
--                         , s
--                         )
--                     Nothing ->
--                         ( Err NotEnoughSource, source )
--         in
--         ( \template ->
--             { template
--                 | earTip =
--                     Result.map2
--                         (\crown baseVec ->
--                             crown
--                                 |> Vector3.plus (Vector3 0.1 0.4 0)
--                                 |> Vector3.plus
--                                     (baseVec
--                                         |> Vector3.scaleByVector (Vector3 0.2 0.4 0.4)
--                                     )
--                         )
--                         template.crown
--                         baseVecResult
--             }
--         , remainingSource
--         )
--     , \source ->
--         let
--             ( baseVecResult, remainingSource ) =
--                 case BinarySource.consumeVectorDimNeg1to1 2 source of
--                     Just ( uVec, s ) ->
--                         ( Ok uVec
--                         , s
--                         )
--                     Nothing ->
--                         ( Err NotEnoughSource, source )
--         in
--         ( \template ->
--             { template
--                 | noseTop =
--                     Result.map2
--                         (\noseBottom baseVec ->
--                             noseBottom
--                                 |> Vector3.plus (Vector3 0 0.1 0)
--                                 |> Vector3.plus
--                                     (baseVec
--                                         |> Vector3.scaleBy 0.3
--                                     )
--                         )
--                         template.noseBottom
--                         baseVecResult
--             }
--         , remainingSource
--         )
--     ]
-- oldColoringTransformGenerators : List (BinarySource -> ( Transformer ColoringTemplate, BinarySource ))
-- oldColoringTransformGenerators =
--     [ \source ->
--         ( \template ->
--             { template
--                 | eyequad = Ok Color.darkOrange
--                 , noseBridge = Ok Color.brown
--                 , noseSide = Ok Color.lightBrown
--                 , forehead = Ok Color.orange
--                 , crown = Ok Color.lightOrange
--                 , temple = Ok Color.lightOrange
--                 , earFront = Ok Color.black
--                 , earBack = Ok Color.lightRed
--                 , cheek = Ok Color.brown
--                 , cheekSpot = Ok Color.darkOrange
--                 , chin = Ok Color.white
--             }
--         , source
--         )
--     , \source ->
--         let
--             ( colorResult, remainingSource ) =
--                 case consumeColorFromPallette source of
--                     Ok ( color, s ) ->
--                         ( Ok color, s )
--                     Err err ->
--                         ( Err err, source )
--         in
--         ( \template ->
--             { template
--                 | forehead = colorResult
--             }
--         , remainingSource
--         )
--     , \source ->
--         ( \template ->
--             { template
--                 | eyequad =
--                     template.forehead
--                         |> Result.map
--                             (addVectorToColor (Vector3 0 0 0.6))
--             }
--         , source
--         )
--     ]
