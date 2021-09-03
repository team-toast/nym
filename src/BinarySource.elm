module BinarySource exposing (BinaryChunk, BinarySource, consumeChunk, consumeFloat0to1, consumeIntWithBits, consumeIntWithMax, consumeVectorDimNeg1to1, empty, fromBitsString, unsafeFromBitsString)

import Quantity
import String
import TupleHelpers
import Types exposing (..)
import UInt64
import UInt64.Digits as UInt64
import Utils exposing (..)
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


consumeChunk : Int -> BinarySource -> Maybe ( BinaryChunk, BinarySource )
consumeChunk numBits (BinarySource source) =
    if String.length source >= numBits then
        Just
            ( BinaryChunk <| String.left numBits source
            , BinarySource <| String.dropLeft numBits source
            )

    else
        Nothing


consumeIntWithBits : Int -> BinarySource -> Maybe ( Int, BinarySource )
consumeIntWithBits bits source =
    source
        |> consumeChunk bits
        |> Maybe.map (Tuple.mapFirst chunkToInt32)


consumeIntWithMax : Int -> BinarySource -> Maybe ( Int, BinarySource )
consumeIntWithMax max source =
    let
        bitsNeeded =
            max
                |> toFloat
                |> (logBase 2 >> (+) 1)
                |> floor
    in
    consumeIntWithBits bitsNeeded source


consumeFloat0to1 : Int -> BinarySource -> Maybe ( Float, BinarySource )
consumeFloat0to1 bits source =
    source
        |> consumeIntWithBits bits
        |> Maybe.map
            (Tuple.mapFirst
                (\divisorInt ->
                    toFloat divisorInt
                        / (toFloat <| 2 ^ bits - 1)
                )
            )


consumeVectorDimNeg1to1 : Int -> BinarySource -> Maybe ( Vector3, BinarySource )
consumeVectorDimNeg1to1 bitsPerComponent source =
    consumeFloat0to1 bitsPerComponent source
        |> Maybe.andThen
            (\( x, source1 ) ->
                consumeFloat0to1 bitsPerComponent source1
                    |> Maybe.andThen
                        (\( y, source2 ) ->
                            consumeFloat0to1 bitsPerComponent source2
                                |> Maybe.andThen
                                    (\( z, source3 ) ->
                                        Just
                                            ( Vector3 x y z
                                                |> Vector3.scaleBy 2
                                                |> Vector3.minus (Vector3 1 1 1)
                                            , source3
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
