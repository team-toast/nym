module BinarySource exposing (BinaryChunk, BinarySource, consumeBits, empty, fromBitsString, unsafeFromBitsString)

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
        Just <| unsafeFromBitsString str

    else
        Nothing


unsafeFromBitsString : String -> BinarySource
unsafeFromBitsString str =
    BinarySource str


consumeBits : Int -> BinarySource -> Maybe ( BinaryChunk, BinarySource )
consumeBits numBits (BinarySource source) =
    if String.length source <= numBits then
        Just
            ( BinaryChunk <| String.dropRight numBits source
            , BinarySource <| String.dropLeft numBits source
            )

    else
        Nothing
