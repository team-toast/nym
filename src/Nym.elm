module Nym exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
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
            Scene3d.group
                [ Scene3d.quad
                    (Material.color Color.lightGray)
                    nym.structure.noseTop
                    nym.structure.noseMid
                    (mirrorPoint nym.structure.noseMid)
                    (mirrorPoint nym.structure.noseTop)
                , Scene3d.quad
                    (Material.color Color.lightGray)
                    nym.structure.noseMid
                    nym.structure.noseBottom
                    (mirrorPoint nym.structure.noseBottom)
                    (mirrorPoint nym.structure.noseMid)
                , Scene3d.quad
                    (Material.color Color.lightGray)
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
                (Material.color Color.white)
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


binarySourceToNym : BinarySource -> Nym
binarySourceToNym source =
    let
        ( structure, rSource1 ) =
            consumeStructure source

        ( eye, rSource2 ) =
            consumeEye rSource1

        ( coloring, rSource3 ) =
            consumeColoring rSource2
                |> Maybe.withDefault
                    ( allBlackColoring, source )
    in
    Nym
        structure
        eye
        coloring


consumeStructure : BinarySource -> ( Structure, BinarySource )
consumeStructure source =
    ( testStructure, source )


consumeEye : BinarySource -> ( Eye, BinarySource )
consumeEye source =
    ( testEye, source )


consumeColoring : BinarySource -> Maybe ( Coloring, BinarySource )
consumeColoring fullSource =
    let
        source0 =
            -- just for consistency in the following lines
            fullSource

        -- ( color, source1 ) =
        --     consumeColor source0
        --         |> Maybe.withDefault
        --             (let
        --                 _ =
        --                     Debug.log "failed to consume color"
        --              in
        --              ( Color.black, source0 )
        --             )
        -- remainingSource =
        --     -- change me manually!
        --     source1
    in
    consumeColorFromPallette fullSource
        |> Maybe.map
            (\( color, remainingSource ) ->
                ( { testColoring
                    | eyequad = color
                  }
                , remainingSource
                )
            )


consumeColorFromPallette : BinarySource -> Maybe ( Color, BinarySource )
consumeColorFromPallette source =
    BinarySource.consumeIntWithMax (List.length allColors - 1) source
        |> Maybe.map
            (Tuple.mapFirst
                (\colorNum ->
                    List.Extra.getAt colorNum allColors
                )
            )
        |> (\weirdMaybe ->
                case weirdMaybe of
                    Just ( Just a, b ) ->
                        Just ( a, b )

                    _ ->
                        Nothing
           )
