module Utils exposing (..)

import TupleHelpers
import Color exposing (Color)
import Direction3d
import Length
import List.Extra
import Point3d exposing (Point3d)
import Quantity
import Types exposing (..)
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)
import Vector3d exposing (Vector3d)


colorToRgbVector3 : Color -> Vector3
colorToRgbVector3 color =
    let
        rgba =
            color
                |> Color.toRgba
    in
    Vector3
        rgba.red
        rgba.green
        rgba.blue


rgbVector3ToColor : Vector3 -> Color
rgbVector3ToColor v =
    Color.fromRgba
        { red = v.x
        , green = v.y
        , blue = v.z
        , alpha = 1
        }


addVectorToColor : Vector3 -> Color -> Color
addVectorToColor vector color =
    Vector3.plus
        (colorToRgbVector3 color)
        vector
        |> rgbVector3ToColor


point3dMToVector3dM : Point3d units coordinates -> Vector3d units coordinates
point3dMToVector3dM point =
    Vector3d.from Point3d.origin point


scaleByVector : Vector3d units1 coordinates -> Vector3d units2 coordinates -> Vector3d units1 coordinates
scaleByVector scaleVec vec =
    let
        ( v1, v2 ) =
            ( Vector3d.unwrap scaleVec
            , Vector3d.unwrap vec
            )
    in
    Vector3d.unsafe
        { x = v1.x * v2.x
        , y = v1.y * v2.y
        , z = v1.z * v2.z
        }


mirrorPoint : Vector3 -> Vector3
mirrorPoint v =
    { v
        | x = -v.x
    }


interpolateVector2InQuad : Vector2.Quad -> Vector2 -> Vector2
interpolateVector2InQuad quad v =
    let
        ( yRangeStart, yRangeEnd ) =
            ( ( quad.bottomLeft, quad.bottomRight )
            , ( quad.topLeft, quad.topRight )
            )
                |> TupleHelpers.mapTuple2
                    -- interpolate with xRel to get range of Y values
                    (\( p1, p2 ) ->
                        v.x * (p2.y - p1.y) + p1.y
                    )

        ( xRangeStart, xRangeEnd ) =
            ( ( quad.bottomLeft, quad.topLeft )
            , ( quad.bottomRight, quad.topRight )
            )
                |> TupleHelpers.mapTuple2
                    -- interpolate with xRel to get range of Y values
                    (\( p1, p2 ) ->
                        v.y * (p2.x - p1.x) + p1.x
                    )
    in
    Vector2
        (v.x * (xRangeEnd - xRangeStart) + xRangeStart)
        (v.y * (yRangeEnd - yRangeStart) + yRangeStart)
