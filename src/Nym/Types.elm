module Nym.Types exposing (..)

import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Length




type alias Structure =
    { innerBrow : Point3d Length.Meters ()
    , outerBrow : Point3d Length.Meters ()
    , eyecheek : Point3d Length.Meters ()
    , eyenose : Point3d Length.Meters ()
    }


type alias Eye =
    Point2d Length.Meters ()


type alias Coloring =
    Int


type alias Nym =
    { structure : Structure
    , eye : Eye
    , coloring : Coloring
    }


type alias BinarySource =
    String



-- squashMaybe : a -> Maybe a -> a
-- squashMaybe default maybe =
--     case maybe of
--         Nothing ->
--             let
--                 _ =
--                     Debug.log "Uh oh! Squashing a maybe!" maybe
--             in
--             default
--         Just a ->
--             a
