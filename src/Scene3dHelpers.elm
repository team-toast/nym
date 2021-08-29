module Scene3dHelpers exposing (..)

import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Scene3d


mirrorPoint : Point3d units coordinates -> Point3d units coordinates
mirrorPoint =
    Point3d.mirrorAcross Plane3d.yz


mirrorGroup =
    Scene3d.mirrorAcross Plane3d.yz
