module BinarySource exposing (BinaryChunk, BinarySource, consumeChunk, consumeIntWithBits, consumeIntWithMax, consumeUnitFloat, empty, fromBitsString, unsafeFromBitsString, consumeUnitVector3dU)

import Point3d exposing (Point3d)
import Quantity
import String
import TupleHelpers
import UInt64
import UInt64.Digits as UInt64
import Utils exposing (..)
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


consumeUnitFloat : Int -> BinarySource -> Maybe ( Float, BinarySource )
consumeUnitFloat bits source =
    source
        |> consumeIntWithBits bits
        |> Maybe.map
            (Tuple.mapFirst
                (\divisorInt ->
                    toFloat divisorInt
                        / (toFloat <| 2 ^ bits - 1)
                )
            )


consumeUnitVector3dU : Int -> BinarySource -> Maybe ( Vector3d Quantity.Unitless coordinates, BinarySource )
consumeUnitVector3dU bitsPerComponent source =
    consumeUnitFloat bitsPerComponent source
        |> Maybe.andThen
            (\( xUnit, source1 ) ->
                consumeUnitFloat bitsPerComponent source1
                    |> Maybe.andThen
                        (\( yUnit, source2 ) ->
                            consumeUnitFloat bitsPerComponent source2
                                |> Maybe.andThen
                                    (\( zUnit, source3 ) ->
                                        Just
                                            ( Vector3d.unitless
                                                (xUnit - 0.5)
                                                (yUnit - 0.5)
                                                (zUnit - 0.5)
                                                |> Vector3d.normalize
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
