module BinarySource exposing
    ( BinaryChunk
    , BinarySource
    , allColors
    , andThenConsume
    , consume2
    , consume3
    , consumeChunk
    , consumeColorFromPallette
    , consumeDouble
    , consumeFloat0to1
    , consumeFloatRange
    , consumeIntWithBits
    , consumeSeveralValues
    , consumeTriple
    , consumeUnsignedFloat
    , consumeVector
    , consumeVectorDimNeg1to1
    , consumeVectorFromBounds
    , debugLogAboutToConsume
    , empty
    , fakeConsume
    , fromBitsString
    , getBitsString
    , map
    , remainingBits
    , unsafeFromBitsString
    )

import Color exposing (Color)
import List.Extra
import Maybe.Extra
import String
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


debugLogAboutToConsume : BinarySource -> BinarySource
debugLogAboutToConsume s =
    let
        _ =
            Debug.log "about to consume" (getBitsString s)
    in
    s


getBitsString : BinarySource -> String
getBitsString (BinarySource s) =
    s


fromBitsString : String -> Maybe BinarySource
fromBitsString str =
    if str |> String.all (\c -> c == '1' || c == '0') then
        Just <| unsafeFromBitsString str

    else
        Nothing


unsafeFromBitsString : String -> BinarySource
unsafeFromBitsString str =
    BinarySource str


remainingBits : BinarySource -> Int
remainingBits (BinarySource source) =
    String.length source


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


consumeUnsignedFloat : Int -> Float -> BinarySource -> Maybe ( BinarySource, Float )
consumeUnsignedFloat bits max =
    consumeFloat0to1 bits
        >> map ((*) max)


consumeFloatRange : Int -> ( Float, Float ) -> BinarySource -> Maybe ( BinarySource, Float )
consumeFloatRange bits ( min, max ) =
    consumeFloat0to1 bits
        >> map
            (\uFloat ->
                uFloat * (max - min) + min
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


consumeVector : Int -> Vector3 -> BinarySource -> Maybe ( BinarySource, Vector3 )
consumeVector bitsPerComponent maxVector =
    consumeVectorFromBounds bitsPerComponent
        ( Vector3.zero
        , maxVector
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
    consumeIntWithBits 5 source
        -- happily the list contains exactly 32 items, so 5 bits is perfect
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


consume3 :
    ( BinarySource -> Maybe ( BinarySource, a )
    , BinarySource -> Maybe ( BinarySource, b )
    , BinarySource -> Maybe ( BinarySource, c )
    )
    -> BinarySource
    -> Maybe ( BinarySource, ( a, b, c ) )
consume3 ( f1, f2, f3 ) source =
    source
        |> f1
        |> Maybe.andThen
            (\( s1, v1 ) ->
                s1
                    |> f2
                    |> Maybe.andThen
                        (\( s2, v2 ) ->
                            s2
                                |> f3
                                |> Maybe.map
                                    (\( s3, v3 ) ->
                                        ( s3, ( v1, v2, v3 ) )
                                    )
                        )
            )


consumeTriple : (BinarySource -> Maybe ( BinarySource, a )) -> BinarySource -> Maybe ( BinarySource, ( a, a, a ) )
consumeTriple f source =
    consume3 ( f, f, f ) source


consume2 :
    ( BinarySource -> Maybe ( BinarySource, a )
    , BinarySource -> Maybe ( BinarySource, b )
    )
    -> BinarySource
    -> Maybe ( BinarySource, ( a, b ) )
consume2 ( f1, f2 ) source =
    source
        |> f1
        |> Maybe.andThen
            (\( s1, v1 ) ->
                s1
                    |> f2
                    |> Maybe.map
                        (\( s2, v2 ) ->
                            ( s2, ( v1, v2 ) )
                        )
            )


consumeDouble : (BinarySource -> Maybe ( BinarySource, a )) -> BinarySource -> Maybe ( BinarySource, ( a, a ) )
consumeDouble f source =
    consume2 ( f, f ) source


consumeSeveralValues : Int -> (BinarySource -> Maybe ( BinarySource, valType )) -> BinarySource -> Maybe ( BinarySource, List valType )
consumeSeveralValues count f source =
    let
        func : BinarySource -> () -> ( BinarySource, Maybe valType )
        func s _ =
            f s
                |> Maybe.Extra.unwrap
                    ( s, Nothing )
                    (Tuple.mapSecond Just)

        ( remainingSource, maybeListOfVals ) =
            List.Extra.mapAccuml
                func
                source
                (List.repeat count ())
                |> Tuple.mapSecond Maybe.Extra.combine
    in
    maybeListOfVals
        |> Maybe.map
            (\vals ->
                ( remainingSource, vals )
            )


map : (a -> b) -> Maybe ( BinarySource, a ) -> Maybe ( BinarySource, b )
map f maybeSourceAndVal =
    maybeSourceAndVal
        |> Maybe.map (Tuple.mapSecond f)


andThenConsume : (BinarySource -> Maybe ( BinarySource, b )) -> (a -> b -> c) -> Maybe ( BinarySource, a ) -> Maybe ( BinarySource, c )
andThenConsume consumeFunc mapFunc maybeSourceAndVal =
    maybeSourceAndVal
        |> Maybe.andThen
            (\( source1, aVal ) ->
                consumeFunc source1
                    |> Maybe.map
                        (\( source2, bVal ) ->
                            ( source2, mapFunc aVal bVal )
                        )
            )


fakeConsume : a -> BinarySource -> Maybe ( BinarySource, a )
fakeConsume val source =
    Just ( source, val )



-- consumeSeveralValues : BinarySource -> Int -> (BinarySource -> Maybe ( BinarySource, valType )) -> Maybe ( BinarySource, List valType )
-- consumeSeveralValues source count f =
--     List.range 0 count
--         |> List.Extra.mapAccuml
--             func
--             source
--     let
--         trfunc : () -> Maybe ( BinarySource, List valType ) -> Maybe ( BinarySource, List valType )
--         trfunc _ ( s, vals ) =
--             if List.length vals >= count then
--                 Nothing
--             else
--                 f s
--                     |> Maybe.map
--                         (\(newSource, newVal) ->
--                         )
--         func : ( BinarySource, Int ) -> Maybe ( valType, ( BinarySource, Int ) )
--         func ( s, i ) =
--             if i >= count then
--                 Nothing
--             else
--                 f s
--                     |> Maybe.map
--                         (\( newSource, newVal ) ->
--                             ( newVal, ( newSource, i + 1 ) )
--                         )
--         vals =
--             List.Extra.unfoldr
--                 func
--                 ( source, 0 )
--     in
--     if List.length vals < count then
--         Nothing
--     else
--         Just vals


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
