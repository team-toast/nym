module Transforms exposing (..)

import Angle
import Axis3d exposing (Axis3d)
import BinarySource exposing (BinarySource)
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Length
import LineSegment2d exposing (LineSegment2d)
import List
import List.Extra
import Maybe.Extra
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Result.Extra
import SketchPlane3d exposing (SketchPlane3d)
import TupleHelpers
import Types exposing (..)
import Utils exposing (..)
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)
import Vector3d


consumeEyeQuadAndPupil2d : BinarySource -> Maybe ( BinarySource, Result GenError EyeQuadAndPupil2d )
consumeEyeQuadAndPupil2d source =
    let
        -- _ =
        --     Debug.log "from source" source
        consumeData :
            BinarySource
            ->
                Maybe
                    ( BinarySource
                    , ( ( Vector2, Float, Float )
                      , ( Float, ( ( Float, Float ), ( Float, Float ) ) )
                      )
                    )
        consumeData =
            BinarySource.consume2
                -- pupil
                ( BinarySource.consume3
                    -- 1 point from somewhere in the eyeQuad to draw from
                    ( BinarySource.consumeDouble
                        (BinarySource.consumeFloatRange 2 ( 0.2, 0.8 ))
                        >> BinarySource.map (\( x, y ) -> Vector2 x y)
                      -- from 0 to 3.99, position along the quad's perimeter to put the next point
                    , BinarySource.consumeFloatRange 3 ( 0, 3.999 )
                      -- same units as above and relative to it, how much further along the perimeter to draw the final point
                    , BinarySource.consumeFloatRange 2 ( 0.3, 0.99 )
                      -- note that if the max value is > 1, the code later to construct the pupil will break
                    )
                  -- eyeQuad
                , BinarySource.consume2
                    -- length of bottom line, assumed flat
                    ( BinarySource.consumeFloatRange 2 ( 0.1, 0.4 )
                      -- two top points, relative to the respective point below it
                    , BinarySource.consume2
                        -- top left
                        ( BinarySource.consume2
                            -- x in terms of length of bottom line
                            ( BinarySource.consumeFloatRange 2 ( -0.5, 0.3 )
                              -- y in real world units
                            , BinarySource.consumeFloatRange 2 ( 0.1, 0.25 )
                            )
                          -- top right
                        , BinarySource.consume2
                            -- x in terms of length of bottom line
                            ( BinarySource.consumeFloatRange 2 ( -0.3, 0.5 )
                              -- y in real world units
                            , BinarySource.consumeFloatRange 2 ( 0.1, 0.25 )
                            )
                        )
                    )
                )

        finalConstruct :
            ( ( Vector2, Float, Float )
            , ( Float, ( ( Float, Float ), ( Float, Float ) ) )
            )
            -> Result GenError EyeQuadAndPupil2d
        finalConstruct ( pupilData, quadData ) =
            let
                eyeQuad : Vector2.Quad
                eyeQuad =
                    let
                        ( bottomLength, topPointsData ) =
                            quadData

                        ( bottomLeft, bottomRight ) =
                            ( Vector2 0 0
                            , Vector2 bottomLength 0
                            )

                        ( topLeft, topRight ) =
                            topPointsData
                                |> TupleHelpers.mapTuple2 (Tuple.mapFirst (\xFraction -> xFraction * bottomLength))
                                |> TupleHelpers.mapTuple2 (\( x, y ) -> Vector2 x y)
                                |> Tuple.mapBoth
                                    (Vector2.plus bottomLeft)
                                    (Vector2.plus bottomRight)
                    in
                    Vector2.Quad
                        bottomRight
                        bottomLeft
                        topLeft
                        topRight

                pupilResult : Result GenError Pupil2d
                pupilResult =
                    let
                        ( startPointData, point2PerimeterPos, point3PerimeterRelPos ) =
                            pupilData

                        point1 =
                            startPointData
                                |> interpolateVector2InQuad eyeQuad

                        point3PerimeterPos =
                            point2PerimeterPos
                                + point3PerimeterRelPos
                                |> (\p ->
                                        if p >= 4 then
                                            p - 4

                                        else
                                            p
                                   )

                        ( point2EdgeAndRatio, point3EdgeAndRatio ) =
                            ( point2PerimeterPos, point3PerimeterPos )
                                |> TupleHelpers.mapTuple2
                                    (\perimeterPos ->
                                        let
                                            edgeNum =
                                                floor perimeterPos

                                            edgePosRatio =
                                                perimeterPos - toFloat edgeNum
                                        in
                                        ( edgeNum, edgePosRatio )
                                    )

                        ( maybeEdge1, maybeEdge2 ) =
                            ( point2EdgeAndRatio, point3EdgeAndRatio )
                                |> TupleHelpers.mapTuple2
                                    (\( edgeNum, _ ) ->
                                        Vector2.quadToMetersPolygon eyeQuad
                                            |> Polygon2d.edges
                                            |> List.Extra.getAt edgeNum
                                    )
                    in
                    case ( maybeEdge1, maybeEdge2 ) of
                        ( Just edge1, Just edge2 ) ->
                            let
                                ( point2, lastPoint ) =
                                    ( ( edge1, Tuple.second point2EdgeAndRatio )
                                    , ( edge2, Tuple.second point3EdgeAndRatio )
                                    )
                                        |> TupleHelpers.mapTuple2
                                            (\( edge, edgeRatio ) ->
                                                LineSegment2d.interpolate
                                                    edge
                                                    edgeRatio
                                                    |> Vector2.fromMetersPoint
                                            )
                            in
                            if Tuple.first point2EdgeAndRatio == Tuple.first point3EdgeAndRatio then
                                Ok [ ( point1, point2, lastPoint ) ]

                            else
                                let
                                    -- note that this assumes there is only 1 additional point needed.
                                    -- There maybe more if some of the consume code above is tweaked.
                                    additionalPoint =
                                        LineSegment2d.endPoint edge1 |> Vector2.fromMetersPoint
                                in
                                Ok
                                    [ ( point1
                                      , point2
                                      , additionalPoint
                                      )
                                    , ( point1
                                      , additionalPoint
                                      , lastPoint
                                      )
                                    ]

                        _ ->
                            Err <| UnexpectedNothing "index error when trying to index edges of eyeQuad when constructing pupil"
            in
            Result.map
                (\pupil ->
                    EyeQuadAndPupil2d
                        pupil
                        eyeQuad
                )
                pupilResult
    in
    -- get all of our data first in unit-like values, interpolate/extrapolate in finalConstruct above
    source
        |> consumeData
        |> Maybe.map (Tuple.mapSecond finalConstruct)


coreStructureTransforms : List (BinarySource -> BaseStructureTemplate -> ( BinarySource, BaseStructureTemplate ))
coreStructureTransforms =
    [ \source template ->
        -- eyequad and pupil, in 2d
        source
            |> consumeEyeQuadAndPupil2d
            |> tryApplyToTemplate
                (\eyeQuadAndPupilResult ->
                    { template
                        | eyeQuadAndPupil2d = Result.Extra.join eyeQuadAndPupilResult
                    }
                )

    -- sketchplane of eyequad
    , \source template ->
        source
            |> BinarySource.consume3
                ( -- XY angle of axis to rotate the eyeQuad (do eyes pull back toward top of head or toward side?)
                  BinarySource.consumeFloatRange 2 ( pi, pi / 2 )
                  -- rotation amount (how much do eyes pull back?)
                , BinarySource.consumeFloatRange 2 ( 0, 0.85 * (pi / 2) )
                  -- x distance from center to furthest left point of eyeQuad
                , BinarySource.consumeFloatRange 2 ( 0.1, 0.4 )
                )
            |> tryApplyToTemplate
                (\valResult ->
                    { template
                        | eyeQuadSketchplane =
                            Result.map2
                                (\eyeQuadAndPupil ( rotationAxisAngle, rotationAmount, xOffset ) ->
                                    let
                                        rotateAxis =
                                            Direction3d.xy (Angle.radians rotationAxisAngle)
                                                |> Axis3d.through Point3d.origin

                                        sketchPlaneBeforeTranslate =
                                            SketchPlane3d.xy
                                                |> SketchPlane3d.rotateAround
                                                    rotateAxis
                                                    (Angle.radians rotationAmount)

                                        furthestLeftX =
                                            [ eyeQuadAndPupil.eyeQuad.bottomLeft
                                            , eyeQuadAndPupil.eyeQuad.topLeft
                                            ]
                                                |> List.map Vector2.toMetersPoint
                                                |> List.map (Point3d.on sketchPlaneBeforeTranslate)
                                                |> List.map Vector3.fromMetersPoint
                                                |> List.map .x
                                                |> List.minimum
                                                |> Maybe.withDefault 0

                                        translateRightBy =
                                            -furthestLeftX + xOffset
                                    in
                                    sketchPlaneBeforeTranslate
                                        |> SketchPlane3d.translateIn Direction3d.positiveX (Length.meters translateRightBy)
                                )
                                template.eyeQuadAndPupil2d
                                valResult
                    }
                )
    ]



-- oldCoreStructureTransforms : List (BinarySource -> BaseStructureTemplate -> ( BinarySource, BaseStructureTemplate ))
-- oldCoreStructureTransforms =
--     [ \source template ->
--         -- crownBack X
--         source
--             -- 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
--             |> BinarySource.consumeFloatRange 2
--                 ( 0.2, 0.7 )
--             |> tryApplyToTemplate
--                 (\xResult ->
--                     { template
--                         | crownBack =
--                             Result.map
--                                 (\x ->
--                                     Vector3
--                                         x
--                                         1
--                                         0
--                                 )
--                                 xResult
--                     }
--                 )
--     , \source template ->
--         -- crownFront
--         source
--             |> BinarySource.consumeVectorFromBounds 2
--                 ( Vector3 0.2 0.7 0.4
--                 , Vector3 0.7 1 0.8
--                 )
--             |> tryApplyToTemplate
--                 (\pointResult ->
--                     { template
--                         | crownFront = pointResult
--                     }
--                 )
--     , \source template ->
--         -- innerBrow (forehead)
--         source
--             |> BinarySource.consume3
--                 -- x
--                 ( BinarySource.consumeFloatRange 2
--                     ( 0.1, 0.4 )
--                   -- YZ angle from -Y (from crownFront)
--                 , BinarySource.consumeFloatRange 2
--                     ( 0, pi / 6 )
--                   -- length of angled line (from crownFront)
--                 , BinarySource.consumeFloatRange 2
--                     ( 0.2, 0.8 )
--                 )
--             |> tryApplyToTemplate
--                 (\valsResult ->
--                     { template
--                         | innerBrow =
--                             Result.map2
--                                 (\crownFront ( x, angle, length ) ->
--                                     Vector3
--                                         x
--                                         (crownFront.y + (-length * cos angle))
--                                         (crownFront.z + (length * sin angle))
--                                 )
--                                 template.crownFront
--                                 valsResult
--                     }
--                 )
--     , \source template ->
--         -- outerBrow
--         source
--             -- vector relative to innerBrow
--             |> BinarySource.consumeVectorFromBounds 2
--                 ( Vector3 0.15 -0.2 -0.3
--                 , Vector3 0.5 0.2 0
--                 )
--             |> tryApplyToTemplate
--                 (\relVecResult ->
--                     { template
--                         | outerBrow =
--                             Result.map2
--                                 Vector3.plus
--                                 template.innerBrow
--                                 relVecResult
--                     }
--                 )
--     , \source template ->
--         -- both eyeBottom points
--         -- we first determine x and y info, assume these points land on a vertical plane with the brow points,
--         -- then transform the points by a final angle (angle of eye plane)
--         source
--             |> BinarySource.consume3
--                 -- outerEyeBottom x and y info
--                 ( BinarySource.consume2
--                     -- x (as ratio from innerBrow to outerBrow; >1 indicates farther right than outerBrow)
--                     ( BinarySource.consumeFloatRange 2
--                         ( 0.6, 1.3 )
--                       -- y relative to outerBrow
--                     , BinarySource.consumeFloatRange 2
--                         ( -0.15, -0.4 )
--                     )
--                   -- innerEyeBottom x and y info
--                 , BinarySource.consume2
--                     -- x (as ratio from innerBrow to outerBrow)
--                     ( BinarySource.consumeFloatRange 2
--                         ( 0, 0.4 )
--                       -- y relative to innerBrow
--                     , BinarySource.consumeFloatRange 2
--                         ( -0.15, -0.4 )
--                     )
--                   -- Angle to transform the eye plane around [innerBrow -> outerBrow] line. Positive angles the eyeplane up.
--                 , BinarySource.consumeFloatRange 2
--                     ( 0.5, 0.5 )
--                 )
--             |> tryApplyToTemplate
--                 (\valsResult ->
--                     let
--                         pointsResult =
--                             Result.map3
--                                 (\innerBrow outerBrow ( ( outerXRatio, outerYRel ), ( innerXRatio, innerYRel ), angle ) ->
--                                     let
--                                         upFromOuterBrow =
--                                             Vector3.plus outerBrow (Vector3 0 1 0)
--                                         planeRotateAxis : Axis3d.Axis3d Length.Meters ()
--                                         planeRotateAxis =
--                                             Axis3d.throughPoints
--                                                 (Vector3.toMetersPoint innerBrow)
--                                                 (Vector3.toMetersPoint outerBrow)
--                                                 |> Maybe.andThen (Axis3d.projectOnto Plane3d.zx)
--                                                 |> Maybe.withDefault
--                                                     (let
--                                                         _ =
--                                                             Debug.log "can't make a plane for rotation plane eye face math!"
--                                                      in
--                                                      Axis3d.x
--                                                     )
--                                         sketchPlane =
--                                             SketchPlane3d.throughPoints
--                                                 (innerBrow |> Vector3.toMetersPoint)
--                                                 (outerBrow |> Vector3.toMetersPoint)
--                                                 (upFromOuterBrow |> Vector3.toMetersPoint)
--                                                 |> Maybe.map (SketchPlane3d.rotateAround planeRotateAxis (Angle.radians -angle))
--                                                 |> Maybe.withDefault
--                                                     (let
--                                                         _ =
--                                                             Debug.log "can't make a sketch plane for eye face math!"
--                                                      in
--                                                      SketchPlane3d.xy
--                                                     )
--                                         outerBrow2d =
--                                             (outerBrow |> Vector3.toMetersPoint)
--                                                 |> Point3d.projectInto sketchPlane
--                                         outerBottom2d =
--                                             -- remember innerBrow is at the origin of the sketchplane
--                                             Point2d.meters
--                                                 (outerXRatio
--                                                     * (outerBrow2d |> Point2d.toRecord Length.inMeters |> .x)
--                                                 )
--                                                 outerYRel
--                                         innerBottom2d =
--                                             Point2d.meters
--                                                 (innerXRatio
--                                                     * (outerBrow2d |> Point2d.toRecord Length.inMeters |> .x)
--                                                 )
--                                                 innerYRel
--                                     in
--                                     ( outerBottom2d, innerBottom2d )
--                                         |> TupleHelpers.mapTuple2
--                                             (Point3d.on sketchPlane
--                                                 >> Vector3.fromMetersPoint
--                                             )
--                                 )
--                                 template.innerBrow
--                                 template.outerBrow
--                                 valsResult
--                     in
--                     { template
--                         | outerEyeBottom = pointsResult |> Result.map Tuple.first
--                         , innerEyeBottom = pointsResult |> Result.map Tuple.second
--                     }
--                 )
--     , \source template ->
--         -- outerTop X and Y
--         source
--             |> BinarySource.consume2
--                 ( BinarySource.consumeFloatRange 2
--                     ( 0.7, 1 )
--                 , BinarySource.consumeFloatRange 2
--                     ( 0, 0.7 )
--                 )
--             |> BinarySource.map
--                 (\( x, y ) ->
--                     Vector3 x y 0
--                 )
--             |> tryApplyToTemplate
--                 (\pointResult ->
--                     { template
--                         | outerTop = pointResult
--                     }
--                 )
--     , \source template ->
--         -- jawBottom x and y
--         source
--             |> BinarySource.consume2
--                 ( BinarySource.consumeFloatRange 2
--                     ( 0.4, 1 )
--                 , BinarySource.consumeFloatRange 2
--                     ( -0.7, -1 )
--                 )
--             |> BinarySource.map
--                 (\( x, y ) ->
--                     Vector3 x y 0
--                 )
--             |> tryApplyToTemplate
--                 (\pointResult ->
--                     { template
--                         | jawBottom = pointResult
--                     }
--                 )
--     , \source template ->
--         -- nose y and z
--         source
--             |> BinarySource.consume2
--                 -- y (ratio from innerBrow to jaw)
--                 ( BinarySource.consumeFloatRange 2
--                     ( 0.4, 1 )
--                   -- z (from innerBrow)
--                 , BinarySource.consumeFloatRange 2
--                     ( 0, 0.8 )
--                 )
--             |> tryApplyToTemplate
--                 (\valsResult ->
--                     { template
--                         | noseYandZ =
--                             Result.map3
--                                 (\innerBrow jawBottom ( yRatio, relZ ) ->
--                                     ( (jawBottom.y - innerBrow.y) * yRatio + innerBrow.y
--                                     , innerBrow.z + relZ
--                                     )
--                                 )
--                                 template.innerBrow
--                                 template.jawBottom
--                                 valsResult
--                     }
--                 )
--     -- , \source template ->
--     --     -- testEye
--     --     ( source
--     --     , { template
--     --         | testEye =
--     --             template.brow
--     --                 |> Result.map
--     --                     (Vector3.plus <| Vector3 0.2 0 0)
--     --       }
--     --     )
--     ]


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
