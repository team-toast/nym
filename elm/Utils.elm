module Utils exposing (..)

import List.Extra
import Color exposing (Color)
import Direction3d
import Length
import Point3d exposing (Point3d)
import Quantity
import Types exposing (..)
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


