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
import TupleHelpers
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
            -- Scene3d.nothing
            Scene3d.point
                { radius = Pixels.pixels 10}
                (Material.color Color.black)
                (nymTemplate.structure.outerBrow
                    |> Result.withDefault Vector3.zero
                    |> Vector3.toMetersPoint
                )

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
                (\innerBrow noseTop noseBridgePoint ->
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
                nymTemplate.structure.noseTop
                nymTemplate.structure.noseBridge
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
            Result.map4
                (\innerBrow outerBrow eyecheek eyenose ->
                    meterQuad
                        (defaultAndLogColorError "eyequad" nymTemplate.coloring.eyequad)
                        innerBrow
                        outerBrow
                        eyecheek
                        eyenose
                )
                nymTemplate.structure.innerBrow
                nymTemplate.structure.outerBrow
                nymTemplate.structure.eyecheek
                nymTemplate.structure.eyenose
                |> defaultAndLogEntityError "eyeSquare"

        eyePoint : Scene3d.Entity ()
        eyePoint =
            Result.map
                (\eye ->
                    Scene3d.point
                        { radius = Pixels.pixels 3 }
                        (Material.color Color.black)
                        (eye |> Vector3.toMetersPoint)
                )
                nymTemplate.eye
                |> defaultAndLogEntityError "eyePoint"

        noseSide : Scene3d.Entity ()
        noseSide =
            Result.map3
                (\( innerBrow, noseBridgePoint ) ( eyenose, noseTop ) ( outerTopSnout, outerBottomSnout ) ->
                    Scene3d.group <|
                        List.map
                            (Scene3d.triangle
                                (defaultAndLogColorError "noseside" nymTemplate.coloring.noseSide)
                                << Triangle3d.fromVertices
                                << TupleHelpers.mapTuple3 Vector3.toMetersPoint
                            )
                            [ ( innerBrow
                              , noseBridgePoint
                              , eyenose
                              )
                            , ( noseBridgePoint
                              , noseTop
                              , eyenose
                              )
                            , ( outerTopSnout
                              , noseTop
                              , outerBottomSnout
                              )
                            ]
                )
                (Result.Extra.combineBoth ( nymTemplate.structure.innerBrow, nymTemplate.structure.noseBridge ))
                (Result.Extra.combineBoth ( nymTemplate.structure.eyenose, nymTemplate.structure.noseTop ))
                (Result.Extra.combineBoth ( nymTemplate.structure.outerTopSnout, nymTemplate.structure.outerBottomSnout ))
                |> defaultAndLogEntityError "noseSide"

        lowerSnout : Scene3d.Entity ()
        lowerSnout =
            Result.map4
                (\outerBottomSnout noseBottom noseMid noseTop ->
                    meterQuad
                        (defaultAndLogColorError "lowerSnout" nymTemplate.coloring.chin)
                        outerBottomSnout
                        noseBottom
                        noseMid
                        noseTop
                )
                nymTemplate.structure.outerBottomSnout
                nymTemplate.structure.noseBottom
                nymTemplate.structure.noseMid
                nymTemplate.structure.noseTop
                |> defaultAndLogEntityError "lowerSnout"

        temple : Scene3d.Entity ()
        temple =
            Result.map4
                (\outerTemple outerBrow innerBrow innerTemple ->
                    meterQuad
                        (defaultAndLogColorError "temple" nymTemplate.coloring.temple)
                        outerTemple
                        outerBrow
                        innerBrow
                        innerTemple
                )
                nymTemplate.structure.outerTemple
                nymTemplate.structure.outerBrow
                nymTemplate.structure.innerBrow
                nymTemplate.structure.innerTemple
                |> defaultAndLogEntityError "temple"

        ear : Scene3d.Entity ()
        ear =
            Result.map5
                (\outerTemple innerTemple earTip highCheek crownPoint ->
                    Scene3d.group
                        [ meterQuad
                            (defaultAndLogColorError "earFront" nymTemplate.coloring.earFront)
                            outerTemple
                            innerTemple
                            earTip
                            highCheek
                        , meterQuad
                            (defaultAndLogColorError "earBack" nymTemplate.coloring.earBack)
                            crownPoint
                            innerTemple
                            earTip
                            highCheek
                        ]
                )
                nymTemplate.structure.outerTemple
                nymTemplate.structure.innerTemple
                nymTemplate.structure.earTip
                nymTemplate.structure.highCheek
                nymTemplate.structure.crown
                |> defaultAndLogEntityError "ear"

        cheek : Scene3d.Entity ()
        cheek =
            Scene3d.group <|
                [ Result.map5
                    (\outerTemple highCheek outerBrow midCheek cheekbone ->
                        Scene3d.group <|
                            List.map
                                (Scene3d.triangle
                                    (defaultAndLogColorError "cheek1" nymTemplate.coloring.cheek1)
                                    << Triangle3d.fromVertices
                                    << TupleHelpers.mapTuple3 Vector3.toMetersPoint
                                )
                                [ ( outerTemple
                                  , highCheek
                                  , outerBrow
                                  )
                                , ( outerBrow
                                  , highCheek
                                  , midCheek
                                  )
                                , ( outerBrow
                                  , midCheek
                                  , cheekbone
                                  )
                                ]
                    )
                    nymTemplate.structure.outerTemple
                    nymTemplate.structure.highCheek
                    nymTemplate.structure.outerBrow
                    nymTemplate.structure.midCheek
                    nymTemplate.structure.cheekbone
                    |> defaultAndLogEntityError "cheek1"
                , Result.map5
                    (\outerBrow cheekbone outerTopSnout eyecheek ( eyenose, noseTop ) ->
                        Scene3d.group <|
                            List.map
                                (Scene3d.triangle
                                    (defaultAndLogColorError "cheek2" nymTemplate.coloring.cheek2)
                                    << Triangle3d.fromVertices
                                    << TupleHelpers.mapTuple3 Vector3.toMetersPoint
                                )
                                [ ( outerBrow
                                  , cheekbone
                                  , outerTopSnout
                                  )
                                , ( outerBrow
                                  , outerTopSnout
                                  , eyecheek
                                  )
                                , ( eyecheek
                                  , eyenose
                                  , outerTopSnout
                                  )
                                , ( eyenose
                                  , noseTop
                                  , outerTopSnout
                                  )
                                ]
                    )
                    nymTemplate.structure.outerBrow
                    nymTemplate.structure.cheekbone
                    nymTemplate.structure.outerTopSnout
                    nymTemplate.structure.eyecheek
                    (Result.Extra.combineBoth ( nymTemplate.structure.eyenose, nymTemplate.structure.noseTop ))
                    |> defaultAndLogEntityError "cheek2"
                , Result.map5
                    (\midCheek lowCheek outerBottomSnout cheekbone outerTopSnout ->
                        Scene3d.group <|
                            [ meterQuad
                                (defaultAndLogColorError "cheek3" nymTemplate.coloring.cheek3)
                                midCheek
                                lowCheek
                                outerBottomSnout
                                cheekbone
                            , (Scene3d.triangle
                                (defaultAndLogColorError "cheekSpot" nymTemplate.coloring.cheekSpot)
                                << Triangle3d.fromVertices
                                << TupleHelpers.mapTuple3 Vector3.toMetersPoint
                              )
                                ( cheekbone
                                , outerBottomSnout
                                , outerTopSnout
                                )
                            ]
                    )
                    nymTemplate.structure.midCheek
                    nymTemplate.structure.lowCheek
                    nymTemplate.structure.outerBottomSnout
                    nymTemplate.structure.cheekbone
                    nymTemplate.structure.outerTopSnout
                    |> defaultAndLogEntityError "cheek3"
                ]
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


binarySourceToNym : Bool -> BinarySource -> NymTemplate
binarySourceToNym defaultErrors source =
    let
        ( rSource1, structureTemplate ) =
            Generate.consumeStructureToTemplate source

        ( rSource2, eyeTemplate ) =
            Generate.consumeEyeToTemplate rSource1

        ( rSource3, coloringTemplate ) =
            Generate.consumeColoringToTemplate rSource2
    in
    NymTemplate
        structureTemplate
        eyeTemplate
        coloringTemplate
        |> (if defaultErrors then
                fillTemplateWithDefaults

            else
                identity
           )


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


defaultAndLogColorError : String -> Result GenError Color -> Material.Material coordinates attributes
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


fillTemplateWithDefaults : NymTemplate -> NymTemplate
fillTemplateWithDefaults template =
    { template
        | coloring =
            let
                coloring =
                    template.coloring
            in
            { coloring
                | eyequad = template.coloring.eyequad |> Result.withDefault Color.darkOrange |> Ok
                , noseBridge = template.coloring.noseBridge |> Result.withDefault Color.brown |> Ok
                , noseSide = template.coloring.noseSide |> Result.withDefault Color.lightBrown |> Ok
                , forehead = template.coloring.forehead |> Result.withDefault Color.orange |> Ok
                , crown = template.coloring.crown |> Result.withDefault Color.lightOrange |> Ok
                , temple = template.coloring.temple |> Result.withDefault Color.lightOrange |> Ok
                , earFront = template.coloring.earFront |> Result.withDefault Color.black |> Ok
                , earBack = template.coloring.earBack |> Result.withDefault Color.lightRed |> Ok
                , cheek1 = template.coloring.cheek1 |> Result.withDefault Color.brown |> Ok
                , cheek2 = template.coloring.cheek2 |> Result.withDefault Color.brown |> Ok
                , cheek3 = template.coloring.cheek3 |> Result.withDefault Color.brown |> Ok
                , cheekSpot = template.coloring.cheekSpot |> Result.withDefault Color.darkOrange |> Ok
                , chin = template.coloring.chin |> Result.withDefault Color.white |> Ok
            }
        , structure =
            let
                structure =
                    template.structure
            in
            { structure
                | innerBrow = template.structure.innerBrow |> Result.withDefault (Vector3 0.1 0.2 0.3) |> Ok
                , outerBrow = template.structure.outerBrow |> Result.withDefault (Vector3 0.5 0.15 0.4) |> Ok
                , cheekbone = template.structure.cheekbone |> Result.withDefault (Vector3 0.5 -0.2 0.2) |> Ok
                , eyecheek = template.structure.eyecheek |> Result.withDefault (Vector3 0.4 0 0.3) |> Ok
                , eyenose = template.structure.eyenose |> Result.withDefault (Vector3 0.2 0 0.4) |> Ok
                , noseTop = template.structure.noseTop |> Result.withDefault (Vector3 0.05 -0.4 1) |> Ok
                , noseMid = template.structure.noseMid |> Result.withDefault (Vector3 0.05 -0.5 1) |> Ok
                , noseBottom = template.structure.noseBottom |> Result.withDefault (Vector3 0.05 -0.55 0.9) |> Ok
                , noseBridge = template.structure.noseBridge |> Result.withDefault (Vector3 0.15 0.08 0.45) |> Ok
                , innerTemple = template.structure.innerTemple |> Result.withDefault (Vector3 0.13 0.4 0.3) |> Ok
                , outerTemple = template.structure.outerTemple |> Result.withDefault (Vector3 0.4 0.4 0.2) |> Ok
                , earTip = template.structure.earTip |> Result.withDefault (Vector3 0.4 0.8 0.2) |> Ok
                , highCheek = template.structure.highCheek |> Result.withDefault (Vector3 0.6 0.5 0) |> Ok
                , midCheek = template.structure.midCheek |> Result.withDefault (Vector3 0.7 0 0) |> Ok
                , lowCheek = template.structure.lowCheek |> Result.withDefault (Vector3 0.7 -0.3 0) |> Ok
                , outerTopSnout = template.structure.outerTopSnout |> Result.withDefault (Vector3 0.4 -0.2 0.3) |> Ok
                , outerBottomSnout = template.structure.outerBottomSnout |> Result.withDefault (Vector3 0.4 -0.4 0.3) |> Ok
                , crown = template.structure.crown |> Result.withDefault (Vector3 0.15 0.6 0) |> Ok
            }
    }
