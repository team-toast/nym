module Transforms exposing (..)

import Angle
import BinarySource exposing (BinarySource)
import Color exposing (Color)
import Length
import List
import List.Extra
import Maybe.Extra
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Result.Extra
import SketchPlane3d exposing (SketchPlane3d)
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
        -- innerBrow (forehead)
        source
            |> BinarySource.consume3
                -- x
                ( BinarySource.consumeFloatRange 2
                    ( 0.1, 0.4 )
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
                        | innerBrow =
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
        -- outerBrow
        source
            -- vector relative to innerBrow
            |> BinarySource.consumeVectorFromBounds 2
                ( Vector3 0.15 -0.2 -0.3
                , Vector3 0.5 0.2 0
                )
            |> tryApplyToTemplate
                (\relVecResult ->
                    { template
                        | outerBrow =
                            Result.map2
                                Vector3.plus
                                template.innerBrow
                                relVecResult
                    }
                )
    , \source template ->
        -- both eyeBottom points
        -- we first determine x and y info, assume these points land on a vertical plane with the brow points,
        -- then transform the points by a final angle (angle of eye plane)
        source
            |> BinarySource.consume3
                -- outerEyeBottom x and y info
                ( BinarySource.consume2
                    -- x (as ratio from innerBrow to outerBrow; >1 indicates farther right than outerBrow)
                    ( BinarySource.consumeFloatRange 2
                        ( 0.6, 1.3 )
                      -- y relative to outerBrow
                    , BinarySource.consumeFloatRange 2
                        ( -0.15, -0.4 )
                    )
                  -- innerEyeBottom x and y info
                , BinarySource.consume2
                    -- x (as ratio from innerBrow to outerBrow)
                    ( BinarySource.consumeFloatRange 2
                        ( 0, 0.4 )
                      -- y relative to innerBrow
                    , BinarySource.consumeFloatRange 2
                        ( -0.15, -0.4 )
                    )
                  -- Angle to transform the eye plane around [innerBrow -> outerBrow] line. Positive angles the eyeplane up.
                , BinarySource.consumeFloatRange 2
                    ( 0.5 , 0.5 )
                )
            |> tryApplyToTemplate
                (\valsResult ->
                    let
                        pointsResult =
                            Result.map3
                                (\innerBrow outerBrow ( ( outerXRatio, outerYRel ), ( innerXRatio, innerYRel ), angle ) ->
                                    let
                                        upFromOuterBrow =
                                            Vector3.plus outerBrow (Vector3 0 1 0)

                                        sketchPlane =
                                            SketchPlane3d.throughPoints
                                                (innerBrow |> Vector3.toMetersPoint)
                                                (outerBrow |> Vector3.toMetersPoint)
                                                (upFromOuterBrow |> Vector3.toMetersPoint)
                                                |> Maybe.map (SketchPlane3d.rotateAroundOwn SketchPlane3d.xAxis (Angle.radians -angle))
                                                |> Maybe.withDefault
                                                    (let
                                                        _ =
                                                            Debug.log "can't make a sketch plane for eye face math!"
                                                     in
                                                     SketchPlane3d.xy
                                                    )

                                        outerBrow2d =
                                            (outerBrow |> Vector3.toMetersPoint)
                                                |> Point3d.projectInto sketchPlane

                                        outerBottom2d =
                                            -- remember innerBrow is at the origin of the sketchplane
                                            Point2d.meters
                                                (outerXRatio
                                                    * (outerBrow2d |> Point2d.toRecord Length.inMeters |> .x)
                                                )
                                                outerYRel

                                        innerBottom2d =
                                            Point2d.meters
                                                (innerXRatio
                                                    * (outerBrow2d |> Point2d.toRecord Length.inMeters |> .x)
                                                )
                                                innerYRel
                                    in
                                    ( outerBottom2d, innerBottom2d )
                                        |> TupleHelpers.mapTuple2
                                            (Point3d.on sketchPlane
                                                >> Vector3.fromMetersPoint
                                            )
                                )
                                template.innerBrow
                                template.outerBrow
                                valsResult
                    in
                    { template
                        | outerEyeBottom = pointsResult |> Result.map Tuple.first
                        , innerEyeBottom = pointsResult |> Result.map Tuple.second
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
                -- y (ratio from innerBrow to jaw)
                ( BinarySource.consumeFloatRange 2
                    ( 0.4, 1 )
                  -- z (from innerBrow)
                , BinarySource.consumeFloatRange 2
                    ( 0, 0.8 )
                )
            |> tryApplyToTemplate
                (\valsResult ->
                    { template
                        | noseYandZ =
                            Result.map3
                                (\innerBrow jawBottom ( yRatio, relZ ) ->
                                    ( (jawBottom.y - innerBrow.y) * yRatio + innerBrow.y
                                    , innerBrow.z + relZ
                                    )
                                )
                                template.innerBrow
                                template.jawBottom
                                valsResult
                    }
                )

    -- , \source template ->
    --     -- testEye
    --     ( source
    --     , { template
    --         | testEye =
    --             template.brow
    --                 |> Result.map
    --                     (Vector3.plus <| Vector3 0.2 0 0)
    --       }
    --     )
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
