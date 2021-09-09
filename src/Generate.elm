module Generate exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import List
import List.Extra
import Point3d exposing (Point3d)
import Result.Extra
import Transforms
import Types exposing (..)
import Utils exposing (..)
import Vector3 exposing (Vector3)
import Vector3d


blankColoringTemplate : ColoringTemplate
blankColoringTemplate =
    ColoringTemplate (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet)


blankStructureTemplate : StructureTemplate
blankStructureTemplate =
    StructureTemplate (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet)


blankEyeTemplate : EyeTemplate
blankEyeTemplate =
    Err NotYetSet


consumeColoringToTemplate : BinarySource -> ( BinarySource, ColoringTemplate )
consumeColoringToTemplate fullSource =
    let
        trfunc : (BinarySource -> ColoringTemplate -> ( BinarySource, ColoringTemplate )) -> ( BinarySource, ColoringTemplate ) -> ( BinarySource, ColoringTemplate )
        trfunc tr ( s, te ) =
            tr s te

        remainingSourceAndTemplate =
            List.foldl
                trfunc
                ( fullSource, blankColoringTemplate )
                Transforms.testColorTransforms
    in
    remainingSourceAndTemplate


coloringTemplateFinalizer : ( List GenError, ColoringTemplate ) -> Result (List GenError) Coloring
coloringTemplateFinalizer ( errors, coloringTemplate ) =
    case
        ( ( coloringTemplate.eyequad, coloringTemplate.noseBridge, coloringTemplate.noseSide )
        , ( coloringTemplate.forehead, coloringTemplate.crown, coloringTemplate.temple )
        , ( ( coloringTemplate.earFront, coloringTemplate.earBack )
          , ( coloringTemplate.cheekSpot, coloringTemplate.chin )
          , ( coloringTemplate.cheek1, coloringTemplate.cheek2, coloringTemplate.cheek3 )
          )
        )
    of
        ( ( Ok eyequad, Ok noseBridge, Ok noseSide ), ( Ok forehead, Ok crown, Ok temple ), ( (Ok earFront, Ok earBack), ( Ok cheekSpot, Ok chin ), (Ok cheek1, Ok cheek2, Ok cheek3) ) ) ->
            Ok
                { eyequad = eyequad
                , noseBridge = noseBridge
                , noseSide = noseSide
                , forehead = forehead
                , crown = crown
                , temple = temple
                , earFront = earFront
                , earBack = earBack
                , cheek1 = cheek1
                , cheek2 = cheek2
                , cheek3 = cheek3
                , cheekSpot = cheekSpot
                , chin = chin
                }

        ( ( eyequadResult, noseBridgeResult, noseSideResult ), ( foreheadResult, crownResult, templeResult ), ( (earFrontResult, earBackResult), ( cheekSpotResult, chinResult ), (cheek1Result, cheek2Result, cheek3Result) ) ) ->
            Err
                ([ eyequadResult
                 , noseBridgeResult
                 , noseSideResult
                 , foreheadResult
                 , crownResult
                 , templeResult
                 , earFrontResult
                 , earBackResult
                 , cheek1Result
                 , cheek2Result
                 , cheek3Result
                 , cheekSpotResult
                 , chinResult
                 ]
                    |> Result.Extra.partition
                    |> Tuple.second
                )


structureTemplateFinalizer : ( List GenError, StructureTemplate ) -> Result (List GenError) Structure
structureTemplateFinalizer ( errors, structureTemplate ) =
    case
        ( ( ( structureTemplate.innerBrow, structureTemplate.outerBrow, structureTemplate.cheekbone )
          , ( structureTemplate.eyecheek, structureTemplate.eyenose, structureTemplate.noseTop )
          , ( structureTemplate.noseMid, structureTemplate.noseBottom, structureTemplate.noseBridge )
          )
        , ( ( structureTemplate.outerTemple, structureTemplate.innerTemple, structureTemplate.earTip )
          , ( structureTemplate.highCheek, structureTemplate.midCheek, structureTemplate.lowCheek )
          , ( structureTemplate.outerTopSnout, structureTemplate.outerBottomSnout, structureTemplate.crown )
          )
        )
    of
        ( ( ( Ok innerBrow, Ok outerBrow, Ok cheekbone ), ( Ok eyecheek, Ok eyenose, Ok noseTop ), ( Ok noseMid, Ok noseBottom, Ok noseBridge ) ), ( ( Ok outerTemple, Ok innerTemple, Ok earTip ), ( Ok highCheek, Ok midCheek, Ok lowCheek ), ( Ok outerTopSnout, Ok outerBottomSnout, Ok crown ) ) ) ->
            Ok
                { innerBrow = innerBrow |> Vector3.toMetersPoint
                , outerBrow = outerBrow |> Vector3.toMetersPoint
                , cheekbone = cheekbone |> Vector3.toMetersPoint
                , eyecheek = eyecheek |> Vector3.toMetersPoint
                , eyenose = eyenose |> Vector3.toMetersPoint
                , noseTop = noseTop |> Vector3.toMetersPoint
                , noseMid = noseMid |> Vector3.toMetersPoint
                , noseBottom = noseBottom |> Vector3.toMetersPoint
                , noseBridge = noseBridge |> Vector3.toMetersPoint
                , outerTemple = outerTemple |> Vector3.toMetersPoint
                , innerTemple = innerTemple |> Vector3.toMetersPoint
                , earTip = earTip |> Vector3.toMetersPoint
                , highCheek = highCheek |> Vector3.toMetersPoint
                , midCheek = midCheek |> Vector3.toMetersPoint
                , lowCheek = lowCheek |> Vector3.toMetersPoint
                , outerTopSnout = outerTopSnout |> Vector3.toMetersPoint
                , outerBottomSnout = outerBottomSnout |> Vector3.toMetersPoint
                , crown = crown |> Vector3.toMetersPoint
                }

        ( ( ( innerBrowResult, outerBrowResult, cheekboneResult ), ( eyecheekResult, eyenoseResult, noseTopResult ), ( noseMidResult, noseBottomResult, noseBridgeResult ) ), ( ( outerTempleResult, innerTempleResult, earTipResult ), ( highCheekResult, midCheekResult, lowCheekResult ), ( outerTopSnoutResult, outerBottomSnoutResult, crownResult ) ) ) ->
            Err
                ([ innerBrowResult
                 , outerBrowResult
                 , cheekboneResult
                 , eyecheekResult
                 , eyenoseResult
                 , noseTopResult
                 , noseMidResult
                 , noseBottomResult
                 , noseBridgeResult
                 , outerTempleResult
                 , innerTempleResult
                 , earTipResult
                 , highCheekResult
                 , midCheekResult
                 , lowCheekResult
                 , outerTopSnoutResult
                 , outerBottomSnoutResult
                 , crownResult
                 ]
                    |> Result.Extra.partition
                    |> Tuple.second
                )


consumeStructureToTemplate : BinarySource -> ( BinarySource, StructureTemplate )
consumeStructureToTemplate fullSource =
    let
        trfunc : (BinarySource -> StructureTemplate -> ( BinarySource, StructureTemplate )) -> ( BinarySource, StructureTemplate ) -> ( BinarySource, StructureTemplate )
        trfunc tr ( s, te ) =
            tr s te

        remainingSourceAndTransformResults =
            List.foldl
                trfunc
                ( fullSource, blankStructureTemplate )
                Transforms.structureTransforms
    in
    remainingSourceAndTransformResults


consumeEyeToTemplate : BinarySource -> ( BinarySource, EyeTemplate )
consumeEyeToTemplate source =
    ( source, blankEyeTemplate )
