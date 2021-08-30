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


testNym =
    Types.testNym


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
                , forehead
                , crown
                ]

        noseBridge : Scene3d.Entity ()
        noseBridge =
            Scene3d.group
                [ Scene3d.quad
                    (Material.color nym.coloring.noseBridge)
                    nym.structure.innerBrow
                    nym.structure.noseBridge
                    (nym.structure.noseBridge |> mirrorPoint)
                    (nym.structure.innerBrow |> mirrorPoint)
                , Scene3d.quad
                    (Material.color nym.coloring.noseBridge)
                    nym.structure.noseBridge
                    nym.structure.nosetop
                    (nym.structure.nosetop |> mirrorPoint)
                    (nym.structure.noseBridge |> mirrorPoint)
                ]

        forehead : Scene3d.Entity ()
        forehead =
            Scene3d.quad
                (Material.color nym.coloring.forehead)
                nym.structure.innerTemple
                nym.structure.innerBrow
                (nym.structure.innerBrow |> mirrorPoint)
                (nym.structure.innerTemple |> mirrorPoint)

        crown : Scene3d.Entity ()
        crown =
            Scene3d.quad
                (Material.color nym.coloring.crown)
                nym.structure.crown
                nym.structure.innerTemple
                (nym.structure.innerTemple |> mirrorPoint)
                (nym.structure.crown |> mirrorPoint)

        copySymmetryGroup =
            Scene3d.group
                [ eyeSquare
                , eyePoint
                , noseSide
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

        noseSide : Scene3d.Entity ()
        noseSide =
            Scene3d.group <|
                List.map
                    (Scene3d.triangle
                        (Material.color nym.coloring.noseSide)
                        << Triangle3d.fromVertices
                    )
                    [ ( nym.structure.innerBrow
                      , nym.structure.noseBridge
                      , nym.structure.eyenose
                      )
                    , ( nym.structure.noseBridge
                      , nym.structure.nosetop
                      , nym.structure.eyenose
                      )
                    ]

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
            Scene3d.group
                [ Scene3d.quad
                    (Material.color nym.coloring.earFront)
                    nym.structure.outerTemple
                    nym.structure.innerTemple
                    nym.structure.earTip
                    nym.structure.highCheek
                , Scene3d.triangle
                    (Material.color nym.coloring.earBack)
                  <|
                    Triangle3d.fromVertices
                        ( nym.structure.innerTemple
                        , nym.structure.crown
                        , nym.structure.earTip
                        )
                ]

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
                    , ( nym.structure.outerBrow
                      , nym.structure.midCheek
                      , nym.structure.outerTopSnout
                      )
                    , ( nym.structure.outerBrow
                      , nym.structure.outerTopSnout
                      , nym.structure.eyecheek
                      )
                    , ( nym.structure.eyecheek
                      , nym.structure.eyenose
                      , nym.structure.outerTopSnout
                      )
                    , ( nym.structure.eyenose
                      , nym.structure.nosetop
                      , nym.structure.outerTopSnout
                      )
                    ]
    in
    allFeatures


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
