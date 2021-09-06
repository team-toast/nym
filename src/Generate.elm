module Generate exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import List
import List.Extra
import Point3d exposing (Point3d)
import Result.Extra
import Types exposing (..)
import Utils exposing (..)
import Vector3 exposing (Vector3)
import Vector3d
import Transforms




applyTransforms : List (Transformer templateType) -> templateType -> templateType
applyTransforms transformers initialTemplate =
    let
        applyTransformerResult : Transformer templateType -> templateType -> templateType
        applyTransformerResult transformer template =
            transformer template
    in
    List.foldl
        applyTransformerResult
        initialTemplate
        transformers


blankColoringTemplate : ColoringTemplate
blankColoringTemplate =
    ColoringTemplate (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet)


blankStructureTemplate : StructureTemplate
blankStructureTemplate =
    StructureTemplate (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet)


blankEyeTemplate : EyeTemplate
blankEyeTemplate =
    Err NotYetSet


indexedColoringTransformGenerator : IndexedTransformGenerator ColoringTemplate
indexedColoringTransformGenerator fullSource transformIndex =
    case List.Extra.getAt transformIndex Transforms.coloringTransformGenerators of
        Just transformerGenerator ->
            let
                ( transformer, remainingSource ) =
                    transformerGenerator fullSource
            in
            ( remainingSource, Ok transformer )

        Nothing ->
            ( fullSource, Err <| InvalidIndex )


consumeColoringToTemplate : BinarySource -> ( List GenError, ColoringTemplate, BinarySource )
consumeColoringToTemplate fullSource =
    let
        remainingSourceAndTransformResults =
            List.Extra.mapAccuml
                indexedColoringTransformGenerator
                fullSource
                (List.range 0 (List.length Transforms.coloringTransformGenerators - 1))

        transformResults : List (Result GenError (Transformer ColoringTemplate))
        transformResults =
            Tuple.second remainingSourceAndTransformResults

        remainingSource =
            Tuple.first remainingSourceAndTransformResults

        ( validTransforms, errors ) =
            Result.Extra.partition transformResults
    in
    ( errors
    , blankColoringTemplate
        |> applyTransforms validTransforms
    , remainingSource
    )


coloringTemplateFinalizer : ( List GenError, ColoringTemplate ) -> Result (List GenError) Coloring
coloringTemplateFinalizer ( errors, coloringTemplate ) =
    case
        ( ( coloringTemplate.eyequad, coloringTemplate.noseBridge, coloringTemplate.noseSide )
        , ( coloringTemplate.forehead, coloringTemplate.crown, coloringTemplate.temple )
        , ( coloringTemplate.earFront, coloringTemplate.earBack, ( coloringTemplate.cheek, coloringTemplate.cheekSpot, coloringTemplate.chin ) )
        )
    of
        ( ( Ok eyequad, Ok noseBridge, Ok noseSide ), ( Ok forehead, Ok crown, Ok temple ), ( Ok earFront, Ok earBack, ( Ok cheek, Ok cheekSpot, Ok chin ) ) ) ->
            Ok
                { eyequad = eyequad
                , noseBridge = noseBridge
                , noseSide = noseSide
                , forehead = forehead
                , crown = crown
                , temple = temple
                , earFront = earFront
                , earBack = earBack
                , cheek = cheek
                , cheekSpot = cheekSpot
                , chin = chin
                }

        ( ( eyequadResult, noseBridgeResult, noseSideResult ), ( foreheadResult, crownResult, templeResult ), ( earFrontResult, earBackResult, ( cheekResult, cheekSpotResult, chinResult ) ) ) ->
            Err
                ([ eyequadResult
                 , noseBridgeResult
                 , noseSideResult
                 , foreheadResult
                 , crownResult
                 , templeResult
                 , earFrontResult
                 , earBackResult
                 , cheekResult
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


consumeStructureToTemplate : BinarySource -> ( List GenError, StructureTemplate, BinarySource )
consumeStructureToTemplate fullSource =
    let
        remainingSourceAndTransformResults =
            List.Extra.mapAccuml
                indexedStructureTransformGenerator
                fullSource
                (List.range 0 (List.length Transforms.structureTransformGenerators - 1))

        transformResults : List (Result GenError (Transformer StructureTemplate))
        transformResults =
            Tuple.second remainingSourceAndTransformResults

        remainingSource =
            Tuple.first remainingSourceAndTransformResults

        ( validTransforms, errors ) =
            Result.Extra.partition transformResults
    in
    ( errors
    , blankStructureTemplate
        |> applyTransforms validTransforms
    , remainingSource
    )


indexedStructureTransformGenerator : IndexedTransformGenerator StructureTemplate
indexedStructureTransformGenerator fullSource transformIndex =
    case List.Extra.getAt transformIndex Transforms.structureTransformGenerators of
        Just transformerGenerator ->
            let
                ( transformer, remainingSource ) =
                    transformerGenerator fullSource
            in
            ( remainingSource, Ok transformer )

        Nothing ->
            ( fullSource, Err <| InvalidIndex )


consumeEyeToTemplate : BinarySource -> ( List GenError, EyeTemplate, BinarySource )
consumeEyeToTemplate source =
    ( [], blankEyeTemplate, source )


consumeColorFromPallette : BinarySource -> Result GenError ( Color, BinarySource )
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
                        Ok ( a, b )

                    Just ( Nothing, b ) ->
                        Err <| OtherError "Color index out of range"

                    Nothing ->
                        Err NotEnoughSource
           )


