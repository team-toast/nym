module Transforms exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import List
import List.Extra
import Point3d exposing (Point3d)
import Result.Extra
import Types exposing (..)
import Utils exposing (..)
import Vector3 exposing (Vector3)
import Vector3d


structureTransformGenerators : List (BinarySource -> ( Transformer StructureTemplate, BinarySource ))
structureTransformGenerators =
    -- these will be sequentially applied to the template.
    -- Any errors should be stored as an Err in whatever was trying to be generated.
    [ \source ->
        -- Middle points, from crown to bottom of nose
        let
            ( crownResult, source1 ) =
                BinarySource.consumeVectorFromBounds 2
                    (Vector3 0 0.4 0)
                    (Vector3 0.5 0.7 0)
                    source
                    |> Maybe.map (Tuple.mapFirst Ok)
                    |> Maybe.withDefault
                        ( Err NotEnoughSource, source )

            remainingSource =
                source1
        in
        ( \template ->
            { template
                | crown = crownResult
                , innerTemple = Ok <| Vector3 0.2 0.4 0.4
                , innerBrow = Ok <| Vector3 0.1 0.2 0.5
                , noseBridge = Ok <| Vector3 0.1 0 0.5
                , noseTop = Ok <| Vector3 0.1 -0.2 1
                , noseMid = Ok <| Vector3 0.1 -0.3 1
                , noseBottom = Ok <| Vector3 0.1 -0.35 0.9
                , outerBottomSnout = Ok <| Vector3 0.3 -0.35 0
            }
        , remainingSource
        )
    ]


coloringTransformGenerators : List (BinarySource -> ( Transformer ColoringTemplate, BinarySource ))
coloringTransformGenerators =
    [ \source ->
        ( \template ->
            { template
                | noseBridge = Ok <| Color.red
            }
        , source
        )
    ]



-- oldStructureTransformGenerators : List (BinarySource -> ( Transformer StructureTemplate, BinarySource ))
-- oldStructureTransformGenerators =
--     [ \source ->
--         ( \template ->
--             { template
--                 | innerBrow = Ok <| Vector3 0.1 0.2 0.3
--                 , outerBrow = Ok <| Vector3 0.5 0.15 0.4
--                 , cheekbone = Ok <| Vector3 0.5 -0.2 0.2
--                 , eyecheek = Ok <| Vector3 0.4 0 0.3
--                 , eyenose = Ok <| Vector3 0.2 0 0.4
--                 , noseTop = Ok <| Vector3 0.05 -0.4 1
--                 , noseMid = Ok <| Vector3 0.05 -0.5 1
--                 , noseBottom = Ok <| Vector3 0.05 -0.55 0.9
--                 , noseBridge = Ok <| Vector3 0.15 0.08 0.45
--                 , innerTemple = Ok <| Vector3 0.13 0.4 0.3
--                 , outerTemple = Ok <| Vector3 0.4 0.4 0.2
--                 , earTip = Ok <| Vector3 0.4 0.8 0.2
--                 , highCheek = Ok <| Vector3 0.6 0.5 0
--                 , midCheek = Ok <| Vector3 0.7 0 0
--                 , lowCheek = Ok <| Vector3 0.7 -0.3 0
--                 , outerTopSnout = Ok <| Vector3 0.4 -0.2 0.3
--                 , outerBottomSnout = Ok <| Vector3 0.4 -0.4 0.3
--                 , crown = Ok <| Vector3 0.15 0.6 0
--             }
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
