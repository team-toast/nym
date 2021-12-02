module Transforms exposing (..)

import Angle
import Axis3d exposing (Axis3d)
import BinarySource exposing (BinarySource)
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Length
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import List
import List.Extra
import Maybe.Extra
import Plane3d exposing (Plane3d)
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


consumeEyeQuadSketchPlane : EyeQuadAndPupil2d -> BinarySource -> Maybe ( BinarySource, SketchPlane3d Length.Meters () {} )
consumeEyeQuadSketchPlane eyeQuadAndPupil2d source =
    source
        |> BinarySource.consume3
            ( -- XY angle of axis to rotate the eyeQuad (do eyes pull back toward top of head or toward side?)
              BinarySource.consumeFloatRange 2 ( pi * 0.9, pi / 2 )
              -- rotation amount (how much do eyes pull back?)
            , BinarySource.consumeFloatRange 2 ( 0, 0.85 * (pi / 2) )
              -- x distance from center to furthest left point of eyeQuad
            , BinarySource.consumeFloatRange 2 ( 0.1, 0.3 )
            )
        |> BinarySource.map
            (\( rotationAxisAngle, rotationAmount, xOffset ) ->
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
                        [ eyeQuadAndPupil2d.eyeQuad.bottomLeft
                        , eyeQuadAndPupil2d.eyeQuad.topLeft
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


consumeEyeQuadAndPupil2d : BinarySource -> Result GenError ( BinarySource, EyeQuadAndPupil2d )
consumeEyeQuadAndPupil2d source =
    let
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
        |> Result.fromMaybe NotEnoughSource
        |> Result.andThen
            (\( s, d ) ->
                case finalConstruct d of
                    Ok f ->
                        Ok ( s, f )

                    Err e ->
                        Err e
            )


consumeFullEyeQuadAndPupil : BinarySource -> Result GenError ( BinarySource, EyeQuadInfo )
consumeFullEyeQuadAndPupil source =
    source
        |> consumeEyeQuadAndPupil2d
        |> Result.andThen
            (\( source1, eyeQuadAndPupil2d ) ->
                source1
                    |> consumeEyeQuadSketchPlane eyeQuadAndPupil2d
                    |> Result.fromMaybe NotEnoughSource
                    |> Result.map
                        (Tuple.mapSecond
                            (\sketchPlane ->
                                let
                                    to3d offset v2 =
                                        Point3d.on
                                            (sketchPlane
                                                |> SketchPlane3d.offsetBy (Length.meters offset)
                                            )
                                            (Vector2.toMetersPoint v2)
                                            |> Vector3.fromMetersPoint
                                in
                                EyeQuadInfo
                                    sketchPlane
                                    { bottomRight = eyeQuadAndPupil2d.eyeQuad.bottomRight |> to3d 0
                                    , bottomLeft = eyeQuadAndPupil2d.eyeQuad.bottomLeft |> to3d 0
                                    , topLeft = eyeQuadAndPupil2d.eyeQuad.topLeft |> to3d 0
                                    , topRight = eyeQuadAndPupil2d.eyeQuad.topRight |> to3d 0
                                    }
                                    (eyeQuadAndPupil2d.pupil
                                        |> List.map (TupleHelpers.mapTuple3 (to3d 0.01))
                                    )
                            )
                        )
            )


coreStructureTransforms : List (BinarySource -> StructureTemplate -> ( BinarySource, StructureTemplate ))
coreStructureTransforms =
    [ -- eyeQuad and pupil
      \source template ->
        case consumeFullEyeQuadAndPupil source of
            Ok ( s, val ) ->
                ( s
                , { template
                    | eyeQuadInfo =
                        Ok val
                  }
                )

            Err e ->
                ( source
                , { template
                    | eyeQuadInfo = Err e
                  }
                )

    -- cheekbone (determined by above, no randomness)
    , \source template ->
        ( source
        , { template
            | cheekbone =
                template.eyeQuadInfo
                    |> Result.map
                        (\eyeQuadInfo ->
                            let
                                vectorOffsetInPlane =
                                    ( Vector3d.from
                                        (eyeQuadInfo.eyeQuad.bottomRight |> Vector3.toMetersPoint)
                                        (eyeQuadInfo.eyeQuad.topRight |> Vector3.toMetersPoint)
                                    , Vector3d.from
                                        (eyeQuadInfo.eyeQuad.bottomRight |> Vector3.toMetersPoint)
                                        (eyeQuadInfo.eyeQuad.bottomLeft |> Vector3.toMetersPoint)
                                    )
                                        |> TupleHelpers.mapTuple2 (Vector3d.scaleBy 0.3)
                                        |> TupleHelpers.mapTuple2 Vector3d.reverse
                                        |> TupleHelpers.combineTuple2 Vector3d.plus

                                vectorOffsetAlongSketchplaneNormal =
                                    SketchPlane3d.normalDirection eyeQuadInfo.sketchPlane
                                        |> Vector3d.withLength (Vector3d.length vectorOffsetInPlane)
                            in
                            Vector3.plus
                                eyeQuadInfo.eyeQuad.bottomRight
                                (Vector3d.plus
                                    vectorOffsetInPlane
                                    vectorOffsetAlongSketchplaneNormal
                                    |> Vector3.fromMetersVector
                                )
                                |> Vector3.plus (Vector3 0 -0.3 0)
                        )
          }
        )

    -- noseTop
    , \source template ->
        source
            |> BinarySource.consume3
                ( -- x offset
                  BinarySource.consumeFloatRange 2 ( 0.03, 0.3 )
                , -- length of line (in YZ plane) from eyeQuad.bottomLeft to this point
                  BinarySource.consumeFloatRange 2 ( 0.2, 0.8 )
                , -- angle of line (in YZ plane) from eyeQuad.bottomLeft to this point, measured downward from +Z
                  BinarySource.consumeFloatRange 2 ( pi / 6, pi / 2 )
                )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    { template
                        | noseTop =
                            Result.map2
                                (\( x, length, angle ) eyeQuadBottomLeft ->
                                    Vector3
                                        x
                                        (eyeQuadBottomLeft.y - (length * cos angle))
                                        (eyeQuadBottomLeft.z + (length * sin angle))
                                )
                                valResult
                                (template.eyeQuadInfo |> Result.map (.eyeQuad >> .bottomLeft))
                    }
                )

    -- noseBridge
    , \source template ->
        source
            |> BinarySource.consume2
                ( -- x as ratio of eyeQuad.topRight.x
                  BinarySource.consumeFloatRange 2 ( 0.1, 0.9 )
                , -- arch of noseBridge where 1 indicates a totally flat bridge
                  BinarySource.consumeFloatRange 2 ( 0.2, 1.5 )
                )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    { template
                        | noseBridge =
                            Result.map3
                                (\( xRatio, archValue ) eyeQuadInfo noseTop ->
                                    let
                                        eyeQuadPoint =
                                            eyeQuadInfo.eyeQuad.bottomLeft

                                        x =
                                            eyeQuadPoint.x * xRatio

                                        straightBridgeLine =
                                            LineSegment3d.fromEndpoints
                                                ( eyeQuadInfo.eyeQuad.topLeft |> Vector3.toMetersPoint
                                                , noseTop |> Vector3.toMetersPoint
                                                )

                                        intermediateBridgePoint =
                                            LineSegment3d.interpolate
                                                straightBridgeLine
                                                0.2
                                                |> Vector3.fromMetersPoint

                                        bridgeOffsetZY =
                                            let
                                                ( eyeQuadPointZY, intermediatPointZY ) =
                                                    ( eyeQuadPoint, intermediateBridgePoint )
                                                        |> TupleHelpers.mapTuple2 (\v3 -> Vector2 v3.z v3.y)
                                            in
                                            Vector2.minus
                                                eyeQuadPointZY
                                                intermediatPointZY
                                                |> (\v ->
                                                        -- in some cases this results in a vector opposite of what we expect.
                                                        -- Here we detect and correct that.
                                                        if v.y + v.x < 0 then
                                                            v |> Vector2.scaleBy -1

                                                        else
                                                            v
                                                   )
                                                |> Vector2.scaleBy archValue
                                    in
                                    Vector3
                                        x
                                        (eyeQuadPoint.y + bridgeOffsetZY.y)
                                        -- remember, here x is actually z :D
                                        (eyeQuadPoint.z + bridgeOffsetZY.x)
                                )
                                valResult
                                template.eyeQuadInfo
                                template.noseTop
                    }
                )

    -- noseBottom
    , \source template ->
        source
            |> -- y distance from noseTop
               BinarySource.consumeFloatRange 2 ( 0.08, 0.2 )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    { template
                        | noseBottom =
                            Result.map2
                                (\yDistance noseTop ->
                                    Vector3
                                        noseTop.x
                                        (noseTop.y - yDistance)
                                        noseTop.z
                                )
                                valResult
                                template.noseTop
                    }
                )

    -- chin
    , \source template ->
        source
            |> BinarySource.consume2
                ( -- x as ratio of noseBottom
                  BinarySource.consumeFloatRange 2 ( 0.5, 1 )
                  -- y distance from nosebottom
                , BinarySource.consumeFloatRange 2 ( 0.05, 0.2 )
                )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    { template
                        | chin =
                            Result.map2
                                (\( xRatio, ySub ) noseBottom ->
                                    Vector3
                                        (noseBottom.x * xRatio)
                                        (noseBottom.y - ySub)
                                        noseBottom.z
                                )
                                valResult
                                template.noseBottom
                    }
                )

    -- crownFront
    , \source template ->
        source
            |> BinarySource.consume3
                ( -- x from midline
                  BinarySource.consumeFloatRange 2 ( 0.03, 0.2 )
                , -- y from highest eyeQuad point
                  BinarySource.consumeFloatRange 2 ( 0.05, 0.2 )
                , -- z units back from eyeQuad.topLeft
                  BinarySource.consumeFloatRange 2 ( 0.1, 0.4 )
                )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    { template
                        | crownFront =
                            Result.map2
                                (\( x, yAdd, zSub ) eyeQuad ->
                                    let
                                        y =
                                            let
                                                highestY =
                                                    max eyeQuad.topRight.y eyeQuad.topLeft.y
                                            in
                                            highestY + yAdd

                                        z =
                                            eyeQuad.topLeft.z - zSub
                                    in
                                    Vector3 x y z
                                )
                                valResult
                                (template.eyeQuadInfo |> Result.map .eyeQuad)
                    }
                )
    , -- backZ
      \source template ->
        source
            -- how far back is the 'back' of the face from the furthest back currently set point?
            |> BinarySource.consumeFloatRange 2 ( 0.1, 0.3 )
            |> tryApplyMaybeValToTemplate
                (\zSubResult ->
                    let
                        furthestBackPointResult =
                            template
                                |> allSetStructurePoints
                                |> Result.map (List.Extra.minimumBy .z)
                                |> Result.map (Result.fromMaybe <| UnexpectedNothing "allSetStructurePoints returning empty list")
                                |> Result.Extra.join
                    in
                    { template
                        | backZ =
                            Result.map2
                                (\furthestBackPoint zSub ->
                                    furthestBackPoint.z - zSub
                                )
                                furthestBackPointResult
                                zSubResult
                    }
                )
    , -- crownBack
      \source template ->
        ( source
        , { template
            | crownBack =
                Result.map2
                    (\crownFront backZ ->
                        { crownFront | z = backZ }
                    )
                    template.crownFront
                    template.backZ
          }
        )
    , --faceSideTop
      \source template ->
        source
            |> BinarySource.consume2
                ( -- x as a ratio of eyeQuad.topRight.x
                  BinarySource.consumeFloatRange 2 ( 0.8, 1.4 )
                , -- y variance from eyeQuad.topRight
                  BinarySource.consumeFloatRange 2 ( -0.15, 0.15 )
                )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    { template
                        | faceSideTop =
                            Result.map3
                                (\( xRatio, yVariance ) eyeQuadTopRight backZ ->
                                    Vector3
                                        (eyeQuadTopRight.x * xRatio)
                                        (eyeQuadTopRight.y + yVariance)
                                        backZ
                                )
                                valResult
                                (template.eyeQuadInfo |> Result.map (.eyeQuad >> .topRight))
                                template.backZ
                    }
                )
    , --faceSideMid
      \source template ->
        source
            |> BinarySource.consume2
                ( -- x distance out from cheekbone
                  BinarySource.consumeFloatRange 2 ( 0, 0.3 )
                , -- y in terms of (faceSideTop.y -> cheekbone.y), where >1 indicates lower than cheekbone.y
                  BinarySource.consumeFloatRange 2 ( 0.6, 1.3 )
                )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    { template
                        | faceSideMid =
                            Result.map4
                                (\( xAdd, yRatio ) cheekbone faceSideTop backZ ->
                                    Vector3
                                        (cheekbone.x + xAdd)
                                        (faceSideTop.y + (cheekbone.y - faceSideTop.y) * yRatio)
                                        backZ
                                )
                                valResult
                                template.cheekbone
                                template.faceSideTop
                                template.backZ
                    }
                )
    , -- jawPoint
      \source template ->
        source
            |> BinarySource.consume3
                ( -- length along (cheekbone -> noseBottom) jawPoint sits under
                  BinarySource.consumeFloatRange 2 ( 0, 0.4 )
                , -- x as a ratio of cheekbone.x
                  BinarySource.consumeFloatRange 2 ( 0.5, 1 )
                , -- y distance from cheekbone
                  BinarySource.consumeFloatRange 2 ( 0.15, 0.2 )
                )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    { template
                        | jawPoint =
                            Result.map3
                                (\( interp, xRatio, ySub ) cheekbone noseBottom ->
                                    let
                                        interpolatedPoint =
                                            Point3d.interpolateFrom
                                                (cheekbone |> Vector3.toMetersPoint)
                                                (noseBottom |> Vector3.toMetersPoint)
                                                interp
                                                |> Vector3.fromMetersPoint
                                    in
                                    { interpolatedPoint
                                        | x = cheekbone.x * xRatio
                                        , y = interpolatedPoint.y - ySub
                                    }
                                )
                                valResult
                                template.cheekbone
                                template.noseBottom
                    }
                )
    , --faceSideBottom
      \source template ->
        source
            |> BinarySource.consume2
                ( -- x as a ratio of faceSideMid
                  BinarySource.consumeFloatRange 2 ( 0.3, 0.9 )
                , -- y down from lowest of (faceSideMid, jawPoint)
                  BinarySource.consumeFloatRange 2 ( 0.1, 0.4 )
                )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    { template
                        | faceSideBottom =
                            Result.map4
                                (\( xRatio, ySub ) faceSideMid jawPoint backZ ->
                                    let
                                        jawOrFaceSidePointY =
                                            min faceSideMid.y jawPoint.y
                                    in
                                    Vector3
                                        (faceSideMid.x * xRatio)
                                        (jawOrFaceSidePointY - ySub)
                                        backZ
                                )
                                valResult
                                template.faceSideMid
                                template.jawPoint
                                template.backZ
                    }
                )

    -- earAttachFrontTop and earAttachFrontBottom
    , \source template ->
        source
            |> BinarySource.consumeInt 1
            |> tryApplyMaybeValToTemplate
                (\coiceResult ->
                    let
                        averagePointResults =
                            Result.map3
                                (\p1 p2 p3 ->
                                    Vector3.plus p1 p2
                                        |> Vector3.plus p3
                                        |> Vector3.scaleBy (1.0 / 3.0)
                                )

                        getTriangleNormalResult p1r p2r p3r =
                            Result.map3
                                (\p1 p2 p3 ->
                                    Vector3d.cross
                                        (Vector3d.from (p2 |> Vector3.toMetersPoint) (p1 |> Vector3.toMetersPoint))
                                        (Vector3d.from (p2 |> Vector3.toMetersPoint) (p3 |> Vector3.toMetersPoint))
                                )
                                p1r
                                p2r
                                p3r
                                |> Result.map (Vector3d.direction >> Result.fromMaybe (UnexpectedNothing "Triangle cross product was a zero vector"))
                                |> Result.Extra.join

                        ( earAttachFrontTop, earAttachFrontBottom, earNormal ) =
                            case coiceResult of
                                Err e ->
                                    ( Err e, Err e, Err e )

                                Ok choice ->
                                    let
                                        eyeQuadPoint =
                                            template.eyeQuadInfo |> Result.map (.eyeQuad >> .topRight)

                                        baseOption1 =
                                            averagePointResults
                                                template.crownBack
                                                template.crownFront
                                                template.faceSideTop

                                        baseOption2 =
                                            averagePointResults
                                                eyeQuadPoint
                                                template.crownFront
                                                template.faceSideTop

                                        baseOption3 =
                                            averagePointResults
                                                eyeQuadPoint
                                                template.faceSideTop
                                                template.faceSideMid
                                    in
                                    case choice of
                                        0 ->
                                            ( baseOption1
                                            , baseOption2
                                            , getTriangleNormalResult
                                                template.crownFront
                                                template.faceSideTop
                                                template.crownBack
                                            )

                                        _ ->
                                            ( baseOption2
                                            , baseOption3
                                            , Result.map2
                                                Direction3d.from
                                                (baseOption2 |> Result.map Vector3.toMetersPoint)
                                                (baseOption3 |> Result.map Vector3.toMetersPoint)
                                                |> Result.map (Result.fromMaybe (UnexpectedNothing "baseOption 2 and 3 coincide"))
                                                |> Result.Extra.join
                                                |> Result.map (Direction3d.rotateAround Axis3d.z (Angle.degrees 90))
                                            )
                    in
                    { template
                        | earAttachFrontTop = earAttachFrontTop
                        , earAttachFrontBottom = earAttachFrontBottom
                        , earBaseNormal = earNormal
                        , earTip =
                            earAttachFrontTop
                                |> Result.map Vector3.toMetersPoint
                                |> Result.map2
                                    (\dir point ->
                                        point |> Point3d.translateIn dir (Length.meters 1)
                                    )
                                    earNormal
                                |> Result.map Vector3.fromMetersPoint
                    }
                )
    ]


otherTry =
    \source template ->
        source
            |> BinarySource.consume2
                ( -- which of two possibilities to use as the base quad to build the ear from
                  BinarySource.consumeBool
                , -- remaining ear construction info in terms of sketchplane based on quad
                  BinarySource.consume2
                    ( -- base points
                      BinarySource.consume2
                        ( -- lower point
                          BinarySource.consume2
                            ( BinarySource.consumeFloatRange 2 ( 0, 1 )
                            , BinarySource.consumeFloatRange 2 ( 0, 0.4 )
                            )
                          -- upper point
                        , BinarySource.consume2
                            ( BinarySource.consumeFloatRange 2 ( 0, 1 )
                            , BinarySource.consumeFloatRange 2 ( 0.6, 1 )
                            )
                        )
                    , -- tip point
                      BinarySource.consume3
                        ( BinarySource.consumeFloatRange 2 ( -1, 2 )
                        , BinarySource.consumeFloatRange 2 ( -1, 2 )
                        , BinarySource.consumeFloatRange 2 ( 0, 1 )
                        )
                    )
                )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    let
                        ( ( earAttachFrontTop, earAttachFrontBottom ), ( earAttachBack, earTip ) ) =
                            case valResult of
                                Err e ->
                                    ( ( Err e, Err e ), ( Err e, Err e ) )

                                Ok ( whichBase, ( ( lowerPointRel, upperPointRel ), tipPointRel ) ) ->
                                    let
                                        sketchPlane =
                                            let
                                                ( ( p1, p2 ), ( p3, p4 ) ) =
                                                    if whichBase then
                                                        Debug.todo ""

                                                    else
                                                        Debug.todo ""
                                            in
                                            SketchPlane3d.throughPoints
                                                (p1 |> Vector3.toMetersPoint)
                                                (p2 |> Vector3.toMetersPoint)
                                                (p3 |> Vector3.toMetersPoint)
                                    in
                                    Debug.todo ""
                    in
                    { template
                        | earAttachFrontTop = earAttachFrontTop
                        , earAttachFrontBottom = earAttachFrontBottom
                        , earAttachBack = earAttachBack
                        , earTip = earTip
                    }
                )


oldEarsTransform =
    \source template ->
        source
            |> BinarySource.emptyConsume ()
            |> tryApplyMaybeValToTemplate
                (\_ ->
                    let
                        roughTemplateSizeResult =
                            template
                                |> getDimensionSizes
                                |> Result.map Vector3.magnitude
                    in
                    { template
                        | earAttachFrontTop = template.crownFront
                        , earAttachFrontBottom =
                            template.faceSideTop
                                |> Result.map (Vector3.plus <| Vector3 -0.3 -0.3 0.4)
                        , earAttachBack = template.crownBack
                        , earTip =
                            Result.map2
                                (\backZ roughTemplateSize ->
                                    Vector3 1 1 backZ
                                        |> Vector3.scaleBy (roughTemplateSize * 0.5)
                                )
                                template.backZ
                                roughTemplateSizeResult
                                |> Result.map (Vector3.plus <| Vector3 0.8 0 0)
                    }
                )


coloringTransforms : List (BinarySource -> ColoringTemplate -> ( BinarySource, ColoringTemplate ))
coloringTransforms =
    let
        consumeMinorVariance =
            BinarySource.consumeVector3ByComponent
                minorVariance

        minorVariance =
            ( ( 1, -0.1, 0.1 )
            , ( 1, -0.1, 0.1 )
            , ( 1, -0.1, 0.1 )
            )
    in
    [ -- eyeQuad (nonrandom for now)
      \source template ->
        ( source
        , { template
            | eyeQuad = Ok Color.white
          }
        )
    , --forehead
      \source template ->
        source
            |> BinarySource.consumeColorFromPallette
            |> tryApplyMaybeValToTemplate
                (\colorResult ->
                    { template
                        | forehead = colorResult
                    }
                )
    , --bridge
      \source template ->
        source
            |> consumeMinorVariance
            |> tryApplyMaybeValToTemplate
                (\varianceResult ->
                    { template
                        | bridge =
                            varyColorResult
                                template.forehead
                                varianceResult
                    }
                )
    , --snoutTop
      \source template ->
        source
            |> consumeMinorVariance
            |> tryApplyMaybeValToTemplate
                (\varianceResult ->
                    { template
                        | snoutTop =
                            varyColorResult
                                template.bridge
                                varianceResult
                    }
                )
    , --crown
      \source template ->
        source
            |> consumeMinorVariance
            |> tryApplyMaybeValToTemplate
                (\varianceResult ->
                    { template
                        | crown =
                            varyColorResult
                                template.forehead
                                varianceResult
                    }
                )
    , --noseTip (nonrandom)
      \source template ->
        ( source
        , { template
            | noseTip = Ok Color.black
          }
        )
    , -- mouth, chinBottom and neck
      \source template ->
        source
            |> BinarySource.consumeColorFromPallette
            |> tryApplyMaybeValToTemplate
                (\colorResult ->
                    { template
                        | mouth = colorResult
                        , chinBottom =
                            colorResult
                                |> Result.map (Utils.scaleColorAndCap 0.9)
                        , neck =
                            colorResult
                                |> Result.map (Utils.scaleColorAndCap 0.8)
                    }
                )
    , --faceSideTop
      \source template ->
        source
            |> BinarySource.consumeColorFromPallette
            |> tryApplyMaybeValToTemplate
                (\colorResult ->
                    { template
                        | faceSideTop = colorResult
                    }
                )
    , --faceSideBottom
      \source template ->
        source
            -- interpolate value from (faceSideTop -> chinBottom)
            |> BinarySource.consumeFloatRange 2 ( 0.1, 0.9 )
            |> tryApplyMaybeValToTemplate
                (\interp ->
                    { template
                        | faceSideBottom =
                            Result.map3
                                Utils.interpolateColors
                                interp
                                template.faceSideTop
                                template.chinBottom
                    }
                )
    , --crownSide
      \source template ->
        source
            -- interpolate value from (faceSideTop -> crown)
            |> BinarySource.consumeFloatRange 2 ( 0.1, 0.9 )
            |> tryApplyMaybeValToTemplate
                (\interp ->
                    { template
                        | crownSide =
                            Result.map3
                                Utils.interpolateColors
                                interp
                                template.faceSideTop
                                template.crown
                    }
                )
    , --belowEar
      \source template ->
        source
            -- interpolate value from (faceSideTop -> crown)
            |> BinarySource.consumeFloatRange 2 ( 0.1, 0.9 )
            |> tryApplyMaybeValToTemplate
                (\interp ->
                    { template
                        | belowEar =
                            Result.map3
                                Utils.interpolateColors
                                interp
                                template.faceSideTop
                                template.crown
                    }
                )

    --aboveEye
    , \source template ->
        source
            -- inherit from forehead or belowEar?
            |> BinarySource.consumeBool
            |> tryApplyMaybeValToTemplate
                (\takeFromForeheadResult ->
                    { template
                        | aboveEye =
                            takeFromForeheadResult
                                |> Result.andThen
                                    (\takeFromForehead ->
                                        if takeFromForehead then
                                            template.forehead

                                        else
                                            template.belowEar
                                    )
                    }
                )
    , --aboveCheekbone
      \source template ->
        source
            -- inherit from aboveEye or faceSideTop?
            |> BinarySource.consumeBool
            |> tryApplyMaybeValToTemplate
                (\takeFromAboveEyeResult ->
                    { template
                        | aboveCheekbone =
                            takeFromAboveEyeResult
                                |> Result.andThen
                                    (\takeFromAboveEye ->
                                        if takeFromAboveEye then
                                            template.aboveEye

                                        else
                                            template.faceSideTop
                                    )
                    }
                )
    , --snoutSideTopMinor
      \source template ->
        source
            |> consumeMinorVariance
            |> tryApplyMaybeValToTemplate
                (\varianceResult ->
                    { template
                        | snoutSideTopMinor =
                            varyColorResult
                                template.snoutTop
                                varianceResult
                    }
                )
    , --snoutSideTopMajor
      \source template ->
        source
            |> BinarySource.consume2
                ( --take from snoutSideTopMinor or faceSideTop?
                  BinarySource.consumeBool
                , consumeMinorVariance
                )
            |> tryApplyMaybeValToTemplate
                (\valResult ->
                    { template
                        | snoutSideTopMajor =
                            Result.map3
                                (\( takeFromSnoutSideTopMinor, variance ) snoutSideTopMinor faceSideTop ->
                                    let
                                        base =
                                            if takeFromSnoutSideTopMinor then
                                                snoutSideTopMinor

                                            else
                                                faceSideTop
                                    in
                                    Utils.addVectorToColorAndWrap variance base
                                )
                                valResult
                                template.snoutSideTopMinor
                                template.faceSideTop
                    }
                )
    , --snoutSideMiddle (nonrandom)
      \source template ->
        ( source
        , { template
            | snoutSideMiddle =
                template.snoutSideTopMajor
                    |> Result.map (Utils.scaleColorAndCap 0.8)
          }
        )
    , --snoutSideBottom
      \source template ->
        source
            |> BinarySource.consumeBool
            |> tryApplyMaybeValToTemplate
                (\boolResult ->
                    { template
                        | snoutSideBottom =
                            boolResult
                                |> Result.andThen
                                    (\bool ->
                                        if bool then
                                            template.snoutSideMiddle
                                                |> Result.map (Utils.scaleColorAndCap 0.9)

                                        else
                                            template.faceSideBottom
                                    )
                    }
                )
    , --jawSide (nonrandom)
      \source template ->
        ( source
        , { template
            | jawSide =
                template.mouth
          }
        )
    ]


varyColorResult : Result a Color -> Result a Vector3 -> Result a Color
varyColorResult cr vr =
    Result.map2
        Utils.addVectorToColorAndWrap
        vr
        cr


tryApplyMaybeValToTemplate :
    (Result GenError val -> template)
    -> Maybe ( BinarySource, val )
    -> ( BinarySource, template )
tryApplyMaybeValToTemplate func maybeSourceAndVal =
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


getDimensionSizes : StructureTemplate -> Result GenError Vector3
getDimensionSizes template =
    getBoundingBox template
        |> Result.map
            (\( start, end ) ->
                end
                    |> Vector3.minus start
                    |> Vector3.absDimensions
            )


getBoundingBox : StructureTemplate -> Result GenError Vector3.RectBounds
getBoundingBox template =
    let
        folder : Vector3 -> Maybe Vector3.RectBounds -> Maybe Vector3.RectBounds
        folder point maybeBounds =
            case maybeBounds of
                Nothing ->
                    Just ( point, point )

                Just ( boundStart, boundEnd ) ->
                    Just
                        ( Vector3
                            (min point.x boundStart.x)
                            (min point.y boundStart.y)
                            (min point.z boundStart.z)
                        , Vector3
                            (max point.x boundEnd.x)
                            (max point.y boundEnd.y)
                            (max point.z boundEnd.z)
                        )
    in
    allSetStructurePoints template
        |> Result.map
            (List.foldl folder Nothing)
        |> Result.map (Result.fromMaybe (UnexpectedNothing "getBoundingBox had no points to iterate over"))
        |> Result.Extra.join
