module Generate exposing (..)

import BinarySource exposing (BinarySource, consumeIntWithBits)
import Color exposing (Color)
import List
import List.Extra
import Point3d exposing (Point3d)
import Result.Extra
import Types exposing (..)
import Utils exposing (..)
import Vector3d


type GenError
    = NotEnoughSource
    | InvalidIndex
    | NotYetSet
    | OtherError String


type alias Transformer templateType =
    templateType -> templateType


type alias TransformerGenResult templateType =
    Result GenError (Transformer templateType)


type alias IndexedTransformGenerator templateType =
    BinarySource -> Int -> ( BinarySource, TransformerGenResult templateType )


applyTransformResults : List (TransformerGenResult templateType) -> (( List GenError, templateType ) -> Result (List GenError) finalType) -> templateType -> Result (List GenError) finalType
applyTransformResults transformerResults templateFinalizer initialTemplate =
    let
        applyTransformerResult : TransformerGenResult templateType -> ( List GenError, templateType ) -> ( List GenError, templateType )
        applyTransformerResult transformerResult ( errors, template ) =
            case transformerResult of
                Ok transformer ->
                    ( errors, transformer template )

                Err newError ->
                    ( List.append
                        errors
                        [ newError ]
                    , template
                    )
    in
    List.foldl
        applyTransformerResult
        ( [], initialTemplate )
        transformerResults
        |> templateFinalizer


type alias ColoringTemplate =
    { eyequad : Result GenError Color
    , noseBridge : Result GenError Color
    , noseSide : Result GenError Color
    , forehead : Result GenError Color
    , crown : Result GenError Color
    , temple : Result GenError Color
    , earFront : Result GenError Color
    , earBack : Result GenError Color
    , cheek : Result GenError Color
    , cheekSpot : Result GenError Color
    , chin : Result GenError Color
    }


blankColoringTemplate : ColoringTemplate
blankColoringTemplate =
    ColoringTemplate (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet)


type alias StructureTemplate =
    { innerBrow : Result GenError Point3dM
    , outerBrow : Result GenError Point3dM
    , cheekbone : Result GenError Point3dM
    , eyecheek : Result GenError Point3dM
    , eyenose : Result GenError Point3dM
    , noseTop : Result GenError Point3dM
    , noseMid : Result GenError Point3dM
    , noseBottom : Result GenError Point3dM
    , noseBridge : Result GenError Point3dM
    , outerTemple : Result GenError Point3dM
    , innerTemple : Result GenError Point3dM
    , earTip : Result GenError Point3dM
    , highCheek : Result GenError Point3dM
    , midCheek : Result GenError Point3dM
    , lowCheek : Result GenError Point3dM
    , outerTopSnout : Result GenError Point3dM
    , outerBottomSnout : Result GenError Point3dM
    , crown : Result GenError Point3dM
    }


blankStructureTemplate : StructureTemplate
blankStructureTemplate =
    StructureTemplate (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet)


indexedColoringTransformGenerator : IndexedTransformGenerator ColoringTemplate
indexedColoringTransformGenerator fullSource transformIndex =
    case List.Extra.getAt transformIndex coloringTransformGenerators of
        Just transformerGenerator ->
            let
                ( transformer, remainingSource ) =
                    transformerGenerator fullSource
            in
            ( remainingSource, Ok transformer )

        Nothing ->
            ( fullSource, Err <| InvalidIndex )


consumeColoring : BinarySource -> ( Result (List GenError) Coloring, BinarySource )
consumeColoring fullSource =
    let
        remainingSourceAndTransformResults =
            List.Extra.mapAccuml
                indexedColoringTransformGenerator
                fullSource
                (List.range 0 (List.length coloringTransformGenerators - 1))

        transformResults : List (Result GenError (Transformer ColoringTemplate))
        transformResults =
            Tuple.second remainingSourceAndTransformResults

        remainingSource =
            Tuple.first remainingSourceAndTransformResults
    in
    ( blankColoringTemplate
        |> applyTransformResults transformResults coloringTemplateFinalizer
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
                { innerBrow = innerBrow
                , outerBrow = outerBrow
                , cheekbone = cheekbone
                , eyecheek = eyecheek
                , eyenose = eyenose
                , noseTop = noseTop
                , noseMid = noseMid
                , noseBottom = noseBottom
                , noseBridge = noseBridge
                , outerTemple = outerTemple
                , innerTemple = innerTemple
                , earTip = earTip
                , highCheek = highCheek
                , midCheek = midCheek
                , lowCheek = lowCheek
                , outerTopSnout = outerTopSnout
                , outerBottomSnout = outerBottomSnout
                , crown = crown
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


structureTransformGenerators : List (BinarySource -> ( Transformer StructureTemplate, BinarySource ))
structureTransformGenerators =
    [ \source ->
        ( \template ->
            { template
                | innerBrow = Ok <| Point3d.meters 0.1 0.2 0.3
                , outerBrow = Ok <| Point3d.meters 0.5 0.15 0.4
                , cheekbone = Ok <| Point3d.meters 0.5 -0.2 0.2
                , eyecheek = Ok <| Point3d.meters 0.4 0 0.3
                , eyenose = Ok <| Point3d.meters 0.2 0 0.4
                , noseTop = Ok <| Point3d.meters 0.05 -0.4 1
                , noseMid = Ok <| Point3d.meters 0.05 -0.5 1
                , noseBottom = Ok <| Point3d.meters 0.05 -0.55 0.9
                , noseBridge = Ok <| Point3d.meters 0.15 0.08 0.45
                , innerTemple = Ok <| Point3d.meters 0.13 0.4 0.3
                , outerTemple = Ok <| Point3d.meters 0.4 0.4 0.2
                , earTip = Ok <| Point3d.meters 0.4 0.8 0.2
                , highCheek = Ok <| Point3d.meters 0.6 0.5 0
                , midCheek = Ok <| Point3d.meters 0.7 0 0
                , lowCheek = Ok <| Point3d.meters 0.7 -0.3 0
                , outerTopSnout = Ok <| Point3d.meters 0.4 -0.2 0.3
                , outerBottomSnout = Ok <| Point3d.meters 0.4 -0.4 0.3
                , crown = Ok <| Point3d.meters 0.15 0.6 0
            }
        , source
        )
    , \source ->
        let
            ( earPointYResult, remainingSource ) =
                case consumeIntWithBits 3 source of
                    Just ( yChangeInt, s ) ->
                        let
                            yChangeRatio =
                                toFloat yChangeInt / 8.0
                        in
                        ( Ok <| (yChangeRatio * 1.0) + 0.4
                        , s
                        )

                    Nothing ->
                        ( Err NotEnoughSource, source )
        in
        ( \template ->
            { template
                | earTip =
                    earPointYResult
                        |> Result.map
                            (\y ->
                                Point3d.meters 0.5 y 0.2
                            )
            }
        , remainingSource
        )
    ]


coloringTransformGenerators : List (BinarySource -> ( Transformer ColoringTemplate, BinarySource ))
coloringTransformGenerators =
    -- these will be sequentially applied to a ColoringTemplate type.
    -- Any errors should be stored as an Err in whatever was trying to be generated.
    [ \source ->
        ( \template ->
            { template
                | eyequad = Ok Color.black
                , noseBridge = Ok Color.black
                , noseSide = Ok Color.black
                , forehead = Ok Color.black
                , crown = Ok Color.black
                , temple = Ok Color.black
                , earFront = Ok Color.black
                , earBack = Ok Color.black
                , cheek = Ok Color.black
                , cheekSpot = Ok Color.black
                , chin = Ok Color.black
            }
        , source
        )
    , \source ->
        let
            ( colorResult, remainingSource ) =
                case consumeColorFromPallette source of
                    Ok ( color, s ) ->
                        ( Ok color, s )

                    Err err ->
                        ( Err err, source )
        in
        ( \template ->
            { template
                | forehead = colorResult
            }
        , remainingSource
        )
    , \source ->
        ( \template ->
            { template
                | eyequad =
                    template.forehead
                        |> Result.map
                            (addVectorToColor (Vector3d.unitless 0 0 0.6))
            }
        , source
        )
    ]


consumeStructure : BinarySource -> ( Result (List GenError) Structure, BinarySource )
consumeStructure fullSource =
    let
        remainingSourceAndTransformResults =
            List.Extra.mapAccuml
                indexedStructureTransformGenerator
                fullSource
                (List.range 0 (List.length structureTransformGenerators - 1))

        transformResults : List (Result GenError (Transformer StructureTemplate))
        transformResults =
            Tuple.second remainingSourceAndTransformResults

        remainingSource =
            Tuple.first remainingSourceAndTransformResults
    in
    ( blankStructureTemplate
        |> applyTransformResults transformResults structureTemplateFinalizer
    , remainingSource
    )


indexedStructureTransformGenerator : IndexedTransformGenerator StructureTemplate
indexedStructureTransformGenerator fullSource transformIndex =
    case List.Extra.getAt transformIndex structureTransformGenerators of
        Just transformerGenerator ->
            let
                ( transformer, remainingSource ) =
                    transformerGenerator fullSource
            in
            ( remainingSource, Ok transformer )

        Nothing ->
            ( fullSource, Err <| InvalidIndex )


consumeEye : BinarySource -> ( Result (List GenError) Eye, BinarySource )
consumeEye source =
    ( Ok testEye, source )


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
