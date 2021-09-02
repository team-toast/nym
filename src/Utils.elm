module Utils exposing (..)

import Color exposing (Color)
import Length
import Vector3d exposing (Vector3d)
import Quantity


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