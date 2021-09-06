module Nym exposing (..)

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
import Utils exposing (..)
import Vector3 exposing (Vector3)
import Vector3d


makeNymEntity : NymTemplate -> Scene3d.Entity ()
makeNymEntity nymTemplate =
    let
        allFeatures =
            Scene3d.group
                [ centerFeatures
                , copySymmetryGroup
                , copiedSymmetryGroup
                , testEntity
                ]

        testEntity =
            Scene3d.nothing

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
            Result.map3
                (\innerBrow noseBridgePoint noseTop ->
                    Scene3d.group
                        [ meterQuad
                            (defaultAndLogColorError "noseBridge" nymTemplate.coloring.noseBridge)
                            innerBrow
                            noseBridgePoint
                            (noseBridgePoint |> mirrorPoint)
                            (innerBrow |> mirrorPoint)
                        , meterQuad
                            (defaultAndLogColorError "noseBridge" nymTemplate.coloring.noseBridge)
                            noseBridgePoint
                            noseTop
                            (noseTop |> mirrorPoint)
                            (noseBridgePoint |> mirrorPoint)
                        ]
                )
                nymTemplate.structure.innerBrow
                nymTemplate.structure.noseBridge
                nymTemplate.structure.noseTop
                |> defaultAndLogEntityError "noseBridge"

        crown : Scene3d.Entity ()
        crown =
            Result.map2
                (\crownPoint innerTemple ->
                    meterQuad
                        (defaultAndLogColorError "crown" nymTemplate.coloring.crown)
                        crownPoint
                        innerTemple
                        (innerTemple |> mirrorPoint)
                        (crownPoint |> mirrorPoint)
                )
                nymTemplate.structure.crown
                nymTemplate.structure.innerTemple
                |> defaultAndLogEntityError "crown"

        noseFront : Scene3d.Entity ()
        noseFront =
            Result.map2
                (\noseTop noseMid ->
                    meterQuad
                        (Material.color Color.black)
                        noseTop
                        noseMid
                        (noseMid |> mirrorPoint)
                        (noseTop |> mirrorPoint)
                )
                nymTemplate.structure.noseTop
                nymTemplate.structure.noseMid
                |> defaultAndLogEntityError "noseFront"

        forehead : Scene3d.Entity ()
        forehead =
            Result.map2
                (\innerTemple innerBrow ->
                    meterQuad
                        (defaultAndLogColorError "forehead" nymTemplate.coloring.forehead)
                        innerTemple
                        innerBrow
                        (innerBrow |> mirrorPoint)
                        (innerTemple |> mirrorPoint)
                )
                nymTemplate.structure.innerTemple
                nymTemplate.structure.innerBrow
                |> defaultAndLogEntityError "forehead"

        chinStrip : Scene3d.Entity ()
        chinStrip =
            let
                chinStripColor =
                    nymTemplate.coloring.chin
                        |> Result.map (addVectorToColor (Vector3 -0.2 -0.2 -0.2))
            in
            Result.map4
                (\noseTop noseMid noseBottom outerBottomSnout ->
                    Scene3d.group
                        [ meterQuad
                            (defaultAndLogColorError "chinStrip" chinStripColor)
                            noseTop
                            noseMid
                            (noseMid |> mirrorPoint)
                            (noseTop |> mirrorPoint)
                        , meterQuad
                            (defaultAndLogColorError "chinStrip" chinStripColor)
                            noseMid
                            noseBottom
                            (noseBottom |> mirrorPoint)
                            (noseMid |> mirrorPoint)
                        , meterQuad
                            (defaultAndLogColorError "chinStrip" chinStripColor)
                            noseBottom
                            outerBottomSnout
                            (outerBottomSnout |> mirrorPoint)
                            (noseBottom |> mirrorPoint)
                        ]
                )
                nymTemplate.structure.noseTop
                nymTemplate.structure.noseMid
                nymTemplate.structure.noseBottom
                nymTemplate.structure.outerBottomSnout
                |> defaultAndLogEntityError "chinStrip"

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
            Scene3d.nothing

        -- Scene3d.quad
        --     (Material.color nymTemplate.coloring.eyequad)
        --     nymTemplate.structure.innerBrow
        --     nymTemplate.structure.outerBrow
        --     nymTemplate.structure.eyecheek
        --     nymTemplate.structure.eyenose
        eyePoint : Scene3d.Entity ()
        eyePoint =
            Scene3d.nothing

        -- Scene3d.point
        --     { radius = Pixels.pixels 3 }
        --     (Material.color Color.black)
        --     nymTemplate.eye
        noseSide : Scene3d.Entity ()
        noseSide =
            Scene3d.nothing

        -- Scene3d.group <|
        --     List.map
        --         (Scene3d.triangle
        --             (Material.color nymTemplate.coloring.noseSide)
        --             << Triangle3d.fromVertices
        --         )
        --         [ ( nymTemplate.structure.innerBrow
        --           , nymTemplate.structure.noseBridge
        --           , nymTemplate.structure.eyenose
        --           )
        --         , ( nymTemplate.structure.noseBridge
        --           , nymTemplate.structure.noseTop
        --           , nymTemplate.structure.eyenose
        --           )
        --         , ( nymTemplate.structure.outerTopSnout
        --           , nymTemplate.structure.noseTop
        --           , nymTemplate.structure.outerBottomSnout
        --           )
        --         ]
        lowerSnout : Scene3d.Entity ()
        lowerSnout =
            Scene3d.nothing

        -- Scene3d.quad
        --     (Material.color nymTemplate.coloring.chin)
        --     nymTemplate.structure.outerBottomSnout
        --     nymTemplate.structure.noseBottom
        --     nymTemplate.structure.noseMid
        --     nymTemplate.structure.noseTop
        temple : Scene3d.Entity ()
        temple =
            Scene3d.nothing

        -- Scene3d.quad
        --     (Material.color nymTemplate.coloring.temple)
        --     nymTemplate.structure.outerTemple
        --     nymTemplate.structure.outerBrow
        --     nymTemplate.structure.innerBrow
        --     nymTemplate.structure.innerTemple
        ear : Scene3d.Entity ()
        ear =
            Scene3d.nothing

        -- Scene3d.group
        --     [ Scene3d.quad
        --         (Material.color nymTemplate.coloring.earFront)
        --         nymTemplate.structure.outerTemple
        --         nymTemplate.structure.innerTemple
        --         nymTemplate.structure.earTip
        --         nymTemplate.structure.highCheek
        --     , Scene3d.quad
        --         (Material.color nymTemplate.coloring.earBack)
        --         nymTemplate.structure.crown
        --         nymTemplate.structure.innerTemple
        --         nymTemplate.structure.earTip
        --         nymTemplate.structure.highCheek
        --     ]
        cheek : Scene3d.Entity ()
        cheek =
            Scene3d.nothing

        -- Scene3d.group <|
        --     List.map
        --         (Scene3d.triangle
        --             (Material.color nymTemplate.coloring.cheek)
        --             << Triangle3d.fromVertices
        --         )
        --         [ ( nymTemplate.structure.outerTemple
        --           , nymTemplate.structure.highCheek
        --           , nymTemplate.structure.outerBrow
        --           )
        --         , ( nymTemplate.structure.outerBrow
        --           , nymTemplate.structure.highCheek
        --           , nymTemplate.structure.midCheek
        --           )
        --         , ( nymTemplate.structure.outerBrow
        --           , nymTemplate.structure.midCheek
        --           , nymTemplate.structure.cheekbone
        --           )
        --         , ( nymTemplate.structure.outerBrow
        --           , nymTemplate.structure.cheekbone
        --           , nymTemplate.structure.outerTopSnout
        --           )
        --         , ( nymTemplate.structure.outerBrow
        --           , nymTemplate.structure.outerTopSnout
        --           , nymTemplate.structure.eyecheek
        --           )
        --         , ( nymTemplate.structure.eyecheek
        --           , nymTemplate.structure.eyenose
        --           , nymTemplate.structure.outerTopSnout
        --           )
        --         , ( nymTemplate.structure.eyenose
        --           , nymTemplate.structure.noseTop
        --           , nymTemplate.structure.outerTopSnout
        --           )
        --         ]
        --         ++ [ Scene3d.quad
        --                 (Material.color nymTemplate.coloring.cheek)
        --                 nymTemplate.structure.midCheek
        --                 nymTemplate.structure.lowCheek
        --                 nymTemplate.structure.outerBottomSnout
        --                 nymTemplate.structure.cheekbone
        --            , Scene3d.triangle
        --                 (Material.color nymTemplate.coloring.cheekSpot)
        --              <|
        --                 Triangle3d.fromVertices
        --                     ( nymTemplate.structure.cheekbone
        --                     , nymTemplate.structure.outerBottomSnout
        --                     , nymTemplate.structure.outerTopSnout
        --                     )
        --            ]
    in
    allFeatures


meterQuad : Material.Textured () -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> Scene3d.Entity ()
meterQuad material v1 v2 v3 v4 =
    Scene3d.quad
        material
        (v1 |> Vector3.toMetersPoint)
        (v2 |> Vector3.toMetersPoint)
        (v3 |> Vector3.toMetersPoint)
        (v4 |> Vector3.toMetersPoint)


binarySourceToNym : BinarySource -> ( List (List GenError), NymTemplate )
binarySourceToNym source =
    let
        ( structureErrors, structureTemplate, rSource1 ) =
            Generate.consumeStructureToTemplate source

        ( eyeErrors, eyeTemplate, rSource2 ) =
            Generate.consumeEyeToTemplate rSource1

        ( coloringErrors, coloringTemplate, rSource3 ) =
            Generate.consumeColoringToTemplate rSource2
    in
    ( [ structureErrors, eyeErrors, coloringErrors ]
    , NymTemplate
        structureTemplate
        eyeTemplate
        coloringTemplate
    )



-- stuff =
--     case ( structureResult, eyeResult, coloringResult ) of
--         ( Ok structure, Ok eye, Ok coloring ) ->
--             Ok <|
--                 Nym
--                     structure
--                     eye
--                     coloring
--         _ ->
--             Err
--                 ([ structureResult |> Result.Extra.error
--                  , eyeResult |> Result.Extra.error
--                  , coloringResult |> Result.Extra.error
--                  ]
--                     |> List.map (Maybe.withDefault [])
--                 )


defaultAndLogEntityError : String -> Result GenError (Scene3d.Entity ()) -> Scene3d.Entity ()
defaultAndLogEntityError name =
    Result.Extra.extract
        (\err ->
            let
                _ =
                    Debug.log ("Entity " ++ name ++ " failed") err
            in
            Scene3d.nothing
        )


defaultAndLogColorError : String -> Result GenError Color -> Material.Textured ()
defaultAndLogColorError name =
    Result.Extra.unpack
        (\err ->
            let
                _ =
                    Debug.log ("Color " ++ name ++ " failed") err
            in
            Material.color Color.black
        )
        Material.color
