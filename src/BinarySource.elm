module BinarySource exposing (BinaryChunk, BinarySource, consumeChunk, consumeColorFromPallette, consumeFloat0to1, consumeIntWithBits, consumeIntWithMax, consumeVectorDimNeg1to1, consumeVectorFromBounds, empty, fromBitsString, unsafeFromBitsString)

import Color exposing (Color)
import List.Extra
import Quantity
import String
import TupleHelpers
import UInt64
import UInt64.Digits as UInt64
import Vector3 exposing (Vector3)
import Vector3d exposing (Vector3d)


type BinarySource
    = BinarySource String


type BinaryChunk
    = BinaryChunk String


empty : BinarySource
empty =
    BinarySource ""


fromBitsString : String -> Maybe BinarySource
fromBitsString str =
    if str |> String.all (\c -> c == '1' || c == '0') then
        Just <| unsafeFromBitsString str

    else
        Nothing


unsafeFromBitsString : String -> BinarySource
unsafeFromBitsString str =
    BinarySource str


consumeChunk : Int -> BinarySource -> Maybe ( BinarySource, BinaryChunk )
consumeChunk numBits (BinarySource source) =
    if String.length source >= numBits then
        Just
            ( BinarySource <| String.dropLeft numBits source
            , BinaryChunk <| String.left numBits source
            )

    else
        Nothing


consumeIntWithBits : Int -> BinarySource -> Maybe ( BinarySource, Int )
consumeIntWithBits bits source =
    source
        |> consumeChunk bits
        |> Maybe.map (Tuple.mapSecond chunkToInt32)


consumeIntWithMax : Int -> BinarySource -> Maybe ( BinarySource, Int )
consumeIntWithMax max source =
    let
        bitsNeeded =
            max
                |> toFloat
                |> (logBase 2 >> (+) 1)
                |> floor
    in
    consumeIntWithBits bitsNeeded source


consumeFloat0to1 : Int -> BinarySource -> Maybe ( BinarySource, Float )
consumeFloat0to1 bits source =
    source
        |> consumeIntWithBits bits
        |> Maybe.map
            (Tuple.mapSecond
                (\divisorInt ->
                    toFloat divisorInt
                        / (toFloat <| 2 ^ bits - 1)
                )
            )


consumeVectorDimNeg1to1 : Int -> BinarySource -> Maybe ( BinarySource, Vector3 )
consumeVectorDimNeg1to1 bitsPerComponent source =
    consumeFloat0to1 bitsPerComponent source
        |> Maybe.andThen
            (\( source1, x ) ->
                consumeFloat0to1 bitsPerComponent source1
                    |> Maybe.andThen
                        (\( source2, y ) ->
                            consumeFloat0to1 bitsPerComponent source2
                                |> Maybe.andThen
                                    (\( source3, z ) ->
                                        Just
                                            ( source3
                                            , Vector3 x y z
                                                |> Vector3.scaleBy 2
                                                |> Vector3.minus (Vector3 1 1 1)
                                            )
                                    )
                        )
            )


consumeVectorFromBounds : Int -> Vector3.RectBounds -> BinarySource -> Maybe ( BinarySource, Vector3 )
consumeVectorFromBounds bitsPerComponent ( boundsStart, boundsEnd ) source =
    consumeFloat0to1 bitsPerComponent source
        |> Maybe.andThen
            (\( source1, x ) ->
                consumeFloat0to1 bitsPerComponent source1
                    |> Maybe.andThen
                        (\( source2, y ) ->
                            consumeFloat0to1 bitsPerComponent source2
                                |> Maybe.andThen
                                    (\( source3, z ) ->
                                        let
                                            boundsSpace =
                                                boundsEnd |> Vector3.minus boundsStart
                                        in
                                        Just
                                            ( source3
                                            , Vector3 x y z
                                                |> Vector3.scaleByVector boundsSpace
                                                |> Vector3.plus boundsStart
                                            )
                                    )
                        )
            )


chunkToInt32 : BinaryChunk -> Int
chunkToInt32 chunk =
    chunk
        |> encodeBinaryString
        |> UInt64.fromString
        |> Maybe.withDefault UInt64.maxSafe
        -- so failure is as ugly and noticeable as possible :D Better than a silent zero...
        |> UInt64.toInt32s
        |> Tuple.second


encodeBinaryString : BinaryChunk -> String
encodeBinaryString (BinaryChunk chunk) =
    "0b" ++ chunk


consumeColorFromPallette : BinarySource -> Maybe ( BinarySource, Color )
consumeColorFromPallette source =
    consumeIntWithMax (List.length allColors - 1) source
        |> Maybe.map
            (Tuple.mapSecond
                (\colorNum ->
                    List.Extra.getAt colorNum allColors
                )
            )
        |> (\weirdMaybe ->
                case weirdMaybe of
                    Just ( a, Just b ) ->
                        Just ( a, b )

                    _ ->
                        Nothing
           )


allColors =
    [ Color.lightRed
    , Color.red
    , Color.darkRed
    , Color.lightOrange
    , Color.orange
    , Color.darkOrange
    , Color.lightYellow
    , Color.yellow
    , Color.darkYellow
    , Color.lightGreen
    , Color.green
    , Color.darkGreen
    , Color.lightBlue
    , Color.blue
    , Color.darkBlue
    , Color.lightPurple
    , Color.purple
    , Color.darkPurple
    , Color.lightBrown
    , Color.brown
    , Color.darkBrown
    , Color.black
    , Color.white
    , Color.lightGrey
    , Color.grey
    , Color.darkGrey
    , Color.lightGray
    , Color.gray
    , Color.darkGray
    , Color.lightCharcoal
    , Color.charcoal
    , Color.darkCharcoal
    ]
