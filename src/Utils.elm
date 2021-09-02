module Utils exposing (..)

import Color exposing (Color)
import Length
import Vector3d exposing (Vector3d)
import Quantity


colorToRgbVector3dM : Color -> Vector3d Quantity.Unitless coordinates
colorToRgbVector3dM color =
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


rgbVector3dMToColor : Vector3d Quantity.Unitless coordinates -> Color
rgbVector3dMToColor vector =
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
