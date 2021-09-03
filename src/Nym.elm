module Nym exposing (..)

import Vector3 exposing (Vector3)
import Utils exposing (..)
import BinarySource exposing (BinarySource)
import Color exposing (Color)
import Generate
import Html exposing (Html)
import Length
import List exposing (range)
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
import Vector3d


makeNymEntity : Nym -> Scene3d.Entity ()
makeNymEntity nym =
    let
        -- todo: rather build a list here and fold? or maybe do this elsewhere to consume features?
        allFeatures =
            Scene3d.group
                [ centerFeatures
                , copySymmetryGroup
                , copiedSymmetryGroup
                , testEntity
                ]

        testEntity =
            Scene3d.nothing

        -- Scene3d.group <|
        --     List.map
        --         (Scene3d.point
        --             { radius = Pixels.pixels 3 }
        --             (Material.color Color.red)
        --         )
        --         [ nym.structure.noseMid
        --         , nym.structure.noseBottom
        --         ]
        centerFeatures : Scene3d.Entity ()
        centerFeatures =
            Scene3d.group
                [ noseBridge
                , noseFront
                , forehead
                , crown
                , chinStrip
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
                    nym.structure.noseTop
                    (nym.structure.noseTop |> mirrorPoint)
                    (nym.structure.noseBridge |> mirrorPoint)
                ]

        noseFront : Scene3d.Entity ()
        noseFront =
            Scene3d.quad
                (Material.color Color.black)
                nym.structure.noseTop
                nym.structure.noseMid
                (mirrorPoint nym.structure.noseMid)
                (mirrorPoint nym.structure.noseTop)

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

        chinStrip : Scene3d.Entity ()
        chinStrip =
            let
                chinStripColor =
                    nym.coloring.chin
                        |> addVectorToColor (Vector3 -0.2 -0.2 -0.2)
            in
            Scene3d.group
                [ Scene3d.quad
                    (Material.color chinStripColor)
                    nym.structure.noseTop
                    nym.structure.noseMid
                    (mirrorPoint nym.structure.noseMid)
                    (mirrorPoint nym.structure.noseTop)
                , Scene3d.quad
                    (Material.color chinStripColor)
                    nym.structure.noseMid
                    nym.structure.noseBottom
                    (mirrorPoint nym.structure.noseBottom)
                    (mirrorPoint nym.structure.noseMid)
                , Scene3d.quad
                    (Material.color chinStripColor)
                    nym.structure.noseBottom
                    nym.structure.outerBottomSnout
                    (mirrorPoint nym.structure.outerBottomSnout)
                    (mirrorPoint nym.structure.noseBottom)
                ]

        copySymmetryGroup =
            Scene3d.group
                [ eyeSquare
                , eyePoint
                , noseSide
                , lowerSnout
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
                      , nym.structure.noseTop
                      , nym.structure.eyenose
                      )
                    , ( nym.structure.outerTopSnout
                      , nym.structure.noseTop
                      , nym.structure.outerBottomSnout
                      )
                    ]

        lowerSnout : Scene3d.Entity ()
        lowerSnout =
            Scene3d.quad
                (Material.color nym.coloring.chin)
                nym.structure.outerBottomSnout
                nym.structure.noseBottom
                nym.structure.noseMid
                nym.structure.noseTop

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
                , Scene3d.quad
                    (Material.color nym.coloring.earBack)
                    nym.structure.crown
                    nym.structure.innerTemple
                    nym.structure.earTip
                    nym.structure.highCheek
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
                      , nym.structure.cheekbone
                      )
                    , ( nym.structure.outerBrow
                      , nym.structure.cheekbone
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
                      , nym.structure.noseTop
                      , nym.structure.outerTopSnout
                      )
                    ]
                    ++ [ Scene3d.quad
                            (Material.color nym.coloring.cheek)
                            nym.structure.midCheek
                            nym.structure.lowCheek
                            nym.structure.outerBottomSnout
                            nym.structure.cheekbone
                       , Scene3d.triangle
                            (Material.color nym.coloring.cheekSpot)
                         <|
                            Triangle3d.fromVertices
                                ( nym.structure.cheekbone
                                , nym.structure.outerBottomSnout
                                , nym.structure.outerTopSnout
                                )
                       ]
    in
    allFeatures


binarySourceToNym : BinarySource -> Result (List (List Generate.GenError)) Nym
binarySourceToNym source =
    let
        ( structureResult, rSource1 ) =
            Generate.consumeStructure source

        ( eyeResult, rSource2 ) =
            Generate.consumeEye rSource1

        ( coloringResult, rSource3 ) =
            Generate.consumeColoring rSource2
    in
    case ( structureResult, eyeResult, coloringResult ) of
        ( Ok structure, Ok eye, Ok coloring ) ->
            Ok <|
                Nym
                    structure
                    eye
                    coloring

        _ ->
            Err
                ([ structureResult |> Result.Extra.error
                 , eyeResult |> Result.Extra.error
                 , coloringResult |> Result.Extra.error
                 ]
                    |> List.map (Maybe.withDefault [])
                )
