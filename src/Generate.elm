module Generate exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import List
import List.Extra
import Result.Extra
import Types exposing (..)


type GenError
    = NotEnoughSource
    | InvalidIndex
    | NotYetSet


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
    }


blankColoringTemplate : ColoringTemplate
blankColoringTemplate =
    ColoringTemplate (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet) (Err NotYetSet)


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
        , ( coloringTemplate.earFront, coloringTemplate.earBack, ( coloringTemplate.cheek, coloringTemplate.cheekSpot ) )
        )
    of
        ( ( Ok eyequad, Ok noseBridge, Ok noseSide ), ( Ok forehead, Ok crown, Ok temple ), ( Ok earFront, Ok earBack, ( Ok cheek, Ok cheekSpot ) ) ) ->
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
                }

        ( ( eyequadResult, noseBridgeResult, noseSideResult ), ( foreheadResult, crownResult, templeResult ), ( earFrontResult, earBackResult, ( cheekResult, cheekSpotResult ) ) ) ->
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
                 ]
                    |> Result.Extra.partition
                    |> Tuple.second
                )


coloringTransformGenerators : List (BinarySource -> ( Transformer ColoringTemplate, BinarySource ))
coloringTransformGenerators =
    -- these will be sequentially applied to a ColoringTemplate type.
    -- Any errors should be stored as an Err in whatever was trying to be generated.
    [ \source ->
        ( \template ->
            { template
                | eyequad = Ok Color.darkOrange
                , noseBridge = Ok Color.brown
                , noseSide = Ok Color.lightBrown
                , forehead = Ok Color.orange
                , crown = Ok Color.lightOrange
                , temple = Ok Color.lightOrange
                , earFront = Ok Color.black
                , earBack = Ok Color.lightRed
                , cheek = Ok Color.brown
                , cheekSpot = Ok Color.darkOrange
            }
        , source
        )
    , \source ->
        ( \template ->
            { template
                | eyequad = Ok Color.red
            }
        , source
        )
    ]


consumeStructure : BinarySource -> ( Result (List GenError) Structure, BinarySource )
consumeStructure source =
    ( Ok testStructure, source )


consumeEye : BinarySource -> ( Result (List GenError) Eye, BinarySource )
consumeEye source =
    ( Ok testEye, source )


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
