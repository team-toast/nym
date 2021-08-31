module BinarySource exposing (BinaryChunk, BinarySource, consumeBits, fromBitsString, empty)

import String


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
        Just <| BinarySource str

    else
        Nothing


consumeBits : Int -> BinarySource -> Maybe ( BinaryChunk, BinarySource )
consumeBits numBits (BinarySource source) =
    if String.length source <= numBits then
        Just
            ( BinaryChunk <| String.dropRight numBits source
            , BinarySource <| String.dropLeft numBits source
            )

    else
        Nothing
