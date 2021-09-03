module Utils exposing (..)

import Color exposing (Color)
import Direction3d
import Length
import Point3d exposing (Point3d)
import Quantity
import Vector3d exposing (Vector3d)


colorToRgbVector3d : Color -> Vector3d Quantity.Unitless coordinates
colorToRgbVector3d color =
    let
        rgba =
            color
                |> Color.toRgba
    in
    Vector3d.fromUnitless
        { x = rgba.red
        , y = rgba.green
        , z = rgba.blue
        }


rgbVector3dToColor : Vector3d Quantity.Unitless coordinates -> Color
rgbVector3dToColor vector =
    let
        xyz =
            vector
                |> Vector3d.toUnitless
    in
    Color.fromRgba
        { red = xyz.x
        , green = xyz.y
        , blue = xyz.z
        , alpha = 1
        }


addVectorToColor : Vector3d Quantity.Unitless coordinates -> Color -> Color
addVectorToColor vector color =
    Vector3d.plus
        (colorToRgbVector3d color)
        vector
        |> rgbVector3dToColor


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
