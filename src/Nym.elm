module Nym exposing (..)

import Color
import Html exposing (Html)
import Length
import List.Extra
import Maybe.Extra
import Pixels
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Result.Extra
import Scene3d
import Scene3d.Material as Material
import Scene3dHelpers exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Triangle3d exposing (Triangle3d)
import Types exposing (..)


makeNymEntity : Nym -> Scene3d.Entity ()
makeNymEntity nym =
    let
        -- todo: rather build a list here and fold? or maybe do this elsewhere to consume features?
        allFeatures =
            Scene3d.group
                [ centerFeatures
                , copySymmetryGroup
                , copiedSymmetryGroup
                ]

        centerFeatures : Scene3d.Entity ()
        centerFeatures =
            Scene3d.group
                [ noseBridge
                ]

        noseBridge : Scene3d.Entity ()
        noseBridge =
            Scene3d.group
                [ Scene3d.quad
                    (Material.color nym.coloring.noseBridge)
                    nym.structure.innerBrow
                    nym.structure.eyenose
                    (nym.structure.eyenose |> mirrorPoint)
                    (nym.structure.innerBrow |> mirrorPoint)
                , Scene3d.quad
                    (Material.color nym.coloring.noseBridge)
                    nym.structure.eyenose
                    nym.structure.nosetop
                    (nym.structure.nosetop |> mirrorPoint)
                    (nym.structure.eyenose |> mirrorPoint)
                ]

        copySymmetryGroup =
            Scene3d.group
                [ eyeSquare
                , eyePoint
                , temple
                , ear
                , cheek
                ]

        copiedSymmetryGroup =
            copySymmetryGroup
                |> mirrorGroup

        eyeSquare =
            Scene3d.quad
                (Material.color nym.coloring.eyequad)
                nym.structure.innerBrow
                nym.structure.outerBrow
                nym.structure.eyecheek
                nym.structure.eyenose

        eyePoint : Scene3d.Entity ()
        eyePoint =
            Scene3d.point
                { radius = Pixels.pixels 3 }
                (Material.color Color.black)
                nym.eye

        temple : Scene3d.Entity ()
        temple =
            Scene3d.quad
                (Material.color nym.coloring.temple)
                nym.structure.outerTemple
                nym.structure.outerBrow
                nym.structure.innerBrow
                nym.structure.innerTemple

        ear : Scene3d.Entity ()
        ear =
            Scene3d.quad
                (Material.color nym.coloring.earFront)
                nym.structure.outerTemple
                nym.structure.innerTemple
                nym.structure.earTip
                nym.structure.highCheek

        cheek : Scene3d.Entity ()
        cheek =
            Scene3d.group <|
                List.map
                    (Scene3d.triangle
                        (Material.color nym.coloring.cheek)
                        << Triangle3d.fromVertices
                    )
                    [ ( nym.structure.outerTemple
                      , nym.structure.highCheek
                      , nym.structure.outerBrow
                      )
                    , ( nym.structure.outerBrow
                      , nym.structure.highCheek
                      , nym.structure.midCheek
                      )
                    ]
    in
    allFeatures


testStructure : Structure
testStructure =
    { innerBrow = Point3d.meters 0.1 0.2 0.3
    , outerBrow = Point3d.meters 0.5 0.15 0.4
    , eyecheek = Point3d.meters 0.4 0 0.3
    , eyenose = Point3d.meters 0.2 0 0.4
    , nosetop = Point3d.meters 0.05 -0.4 1
    , innerTemple = Point3d.meters 0.13 0.4 0.3
    , outerTemple = Point3d.meters 0.4 0.4 0.2
    , earTip = Point3d.meters 0.4 0.8 0.2
    , highCheek = Point3d.meters 0.6 0.5 0
    , midCheek = Point3d.meters 0.7 0 0
    , lowCheek = Point3d.meters 0.7 -0.7 0
    }


testColoring : Coloring
testColoring =
    { eyequad = Color.darkOrange
    , noseBridge = Color.brown
    , temple = Color.lightOrange
    , earFront = Color.black
    , earBack = Color.lightRed
    , cheek = Color.brown
    }


testNym : Nym
testNym =
    Nym
        testStructure
        (Point3d.meters 0.3 0.05 0.4)
        testColoring


binaryStringToNym : BinarySource -> Nym
binaryStringToNym source =
    let
        ( structure, rSource1 ) =
            consumeStructure source

        ( eye, rSource2 ) =
            consumeEye rSource1

        ( coloring, rSource3 ) =
            consumeColoring rSource2
    in
    Nym
        structure
        eye
        coloring


consumeStructure : BinarySource -> ( Structure, BinarySource )
consumeStructure =
    Debug.todo ""


consumeEye : BinarySource -> ( Eye, BinarySource )
consumeEye =
    Debug.todo ""


consumeColoring : BinarySource -> ( Coloring, BinarySource )
consumeColoring =
    Debug.todo ""
