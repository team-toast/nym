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
            -- x values of crown and innerTemple
            |> BinarySource.consumeDouble
                (BinarySource.consumeFloatRange 2 0.1 0.5)
            |> BinarySource.andThenConsume
                (BinarySource.consume2
                    -- crown angle
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
    , \source template ->
        source
            |> BinarySource.consume3
                -- x for brow point
                ( BinarySource.consumeFloatRange 2 0.1 0.3
                  -- forehead angle
                , BinarySource.consumeFloatRange 2 -(pi / 4) -(pi / 2)
                  -- forehead length
                , BinarySource.consumeFloatRange 2 0.1 0.4
                )
            |> tryApplyToTemplate
                (\valsResult ->
                    { template
                        | innerBrow =
                            Result.map2
                                (\innerTemple ( x, angle, length ) ->
                                    Vector3
                                        x
                                        (sin angle * length + innerTemple.y)
                                        (cos angle * length + innerTemple.z)
                                )
                                template.innerTemple
                                valsResult
                    }
                )
    , \source template ->
        -- brow line
        source
            |> BinarySource.consume3
                -- length
                ( BinarySource.consumeFloatRange 2 0.1 0.3
                -- XY angle from +X toward +Y
                , BinarySource.consumeFloatRange 2 -(pi/4) (pi/4)
                -- XZ angle from +X toward -Z
                , BinarySource.consumeFloatRange 2 0 (pi/2)
                )
            |> BinarySource.map
                (\(length, xyAngle, xzAngle) ->
                    Vector3
                        (length * cos xyAngle * cos xzAngle)
                        (length * sin xyAngle)
                        -(length * sin xzAngle)
                )
            |> tryApplyToTemplate
                (\browLineResult ->
                    { template
                        | outerBrow =
                            Result.map2
                                Vector3.plus
                                template.innerBrow
                                browLineResult
                    }
                )
        -- , \source template ->
        --     -- outside bottom eye point
        --     source
        --         |> BinarySource.consume3
        --             -- 'tilt up' angle
        --             ( BinarySource.consumeFloatRange 2 
        --             ,
        --             ,
        --             )
            -- |> BinarySource.consume2
            --     -- plane of eyesquare
            --     ( BinarySource.consume2
            --         -- ZX angle
            --         ( BinarySource.consumeFloatRange 2 0 (pi / 2)
            --           -- ZY angle
            --         , BinarySource.consumeFloatRange 2 -(pi / 8) (pi / 4)
            --         )
            --       -- bottom right point
            --     , BinarySource.consume2
            --         -- angle down from "right"
            --         ( BinarySource.consumeFloatRange 2 (pi / 8) (3 * pi / 8)
            --           -- length
            --         , BinarySource.consumeFloatRange 2 0.1 0.4
            --         )
            --     )
            -- |> BinarySource.andThenConsume
            --     (Debug.todo "")
            --     (\( ( planeZXAngle, planeZYAngle ), ( farPointAngle, farPointLength ) ) wut ->
            --         stuff
            --     )
    , \source template ->
        source
            |> BinarySource.consume3
                -- noseMid
                ( BinarySource.consume3
                    -- x
                    ( BinarySource.consumeFloatRange 2 0.04 0.2
                      -- ZY angle
                    , BinarySource.consumeFloatRange 2 0 -(pi / 4)
                      -- distance from origin
                    , BinarySource.consumeFloatRange 3 0.5 1
                    )
                  -- noseTop
                , BinarySource.consume2
                    -- x
                    ( BinarySource.consumeFloatRange 2 0.04 0.2
                      -- relative y
                    , BinarySource.consumeFloatRange 2 0.06 0.15
                    )
                  -- noseBottom
                , BinarySource.consume3
                    -- x
                    ( BinarySource.consumeFloatRange 2 0.04 0.2
                      -- relative y
                    , BinarySource.consumeFloatRange 1 -0.1 -0.2
                      -- relative z
                    , BinarySource.consumeFloatRange 2 -0.05 -0.2
                    )
                )
            |> BinarySource.map
                (\( ( midX, angle, length ), ( topX, topYRel ), ( bottomX, bottomYRel, bottomZRel ) ) ->
                    let
                        noseMid =
                            Vector3
                                midX
                                (sin angle * length)
                                (cos angle * length)

                        noseTop =
                            Vector3
                                topX
                                (topYRel + noseMid.y)
                                noseMid.z

                        noseBottom =
                            Vector3
                                bottomX
                                (bottomYRel + noseMid.y)
                                (bottomZRel + noseMid.z)
                    in
                    ( noseTop
                    , noseMid
                    , noseBottom
                    )
                )
            |> tryApplyToTemplate
                (\nosePointsResult ->
                    { template
                        | noseTop = nosePointsResult |> Result.map TupleHelpers.tuple3First
                        , noseMid = nosePointsResult |> Result.map TupleHelpers.tuple3Middle
                        , noseBottom = nosePointsResult |> Result.map TupleHelpers.tuple3Last
                    }
                )
    , \source template ->
        -- outerBottomSnout
        source
            |> BinarySource.consumeVectorFromBounds 2
                ( Vector3 0.2 -0.5 0
                , Vector3 0.7 -1 0.5
                )
            |> tryApplyToTemplate
                (\outerBottomSnoutResult ->
                    { template | outerBottomSnout = outerBottomSnoutResult }
                )
    ]


testColoringTransforms : List (BinarySource -> ColoringTemplate -> ( BinarySource, ColoringTemplate ))
testColoringTransforms =
    [ \source template ->
        ( source
        , { template
            | crown = Ok Color.red
            , forehead = Ok Color.red
            , noseBridge = Ok Color.green
            , chin = Ok Color.white
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
