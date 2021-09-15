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


coreStructureTransforms : List (BinarySource -> CoreStructureTemplate -> ( BinarySource, CoreStructureTemplate ))
coreStructureTransforms =
    [ \source template ->
        ( source
        , { template
            | point = Ok <| Vector3 0 0 1
          }
        )
    ]



-- structureTransforms : List (BinarySource -> StructureTemplate -> ( BinarySource, StructureTemplate ))
-- structureTransforms =
--     [ \source template ->
--         source
--             -- x values of crown and innerTemple
--             |> BinarySource.consumeDouble
--                 (BinarySource.consumeFloatRange 2 0.1 0.5)
--             |> BinarySource.andThenConsume
--                 (BinarySource.consume2
--                     -- crown angle
--                     ( BinarySource.consumeFloatRange 2 -(pi / 8) (pi / 8)
--                       -- length of line from crown to forehead
--                     , BinarySource.consumeFloatRange 2 0.1 0.4
--                     )
--                 )
--                 (\( crownX, templeX ) ( yzAngle, crownLength ) ->
--                     let
--                         crown =
--                             Vector3 crownX 0.5 0
--                         temple =
--                             Vector3
--                                 templeX
--                                 (sin yzAngle * crownLength + crown.y)
--                                 (cos yzAngle * crownLength + crown.z)
--                     in
--                     ( crown, temple )
--                 )
--             |> tryApplyToTemplate
--                 (Result.Extra.unpack
--                     (\e ->
--                         { template
--                             | crown = Err e
--                             , innerTemple = Err e
--                         }
--                     )
--                     (\( crown, temple ) ->
--                         { template
--                             | crown = Ok crown
--                             , innerTemple = Ok temple
--                         }
--                     )
--                 )
--     ]


testColoringTransforms : List (BinarySource -> ColoringTemplate -> ( BinarySource, ColoringTemplate ))
testColoringTransforms =
    [ \source template ->
        ( source
        , { template
            | color = Ok Color.red
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
