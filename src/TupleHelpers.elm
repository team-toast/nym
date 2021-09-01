module TupleHelpers exposing (..)

import List.Extra
import Tuple


extractTuple3Maybe : ( Maybe a, Maybe b, Maybe c ) -> Maybe ( a, b, c )
extractTuple3Maybe ( ma, mb, mc ) =
    case ( ma, mb, mc ) of
        ( Just a, Just b, Just c ) ->
            Just ( a, b, c )

        _ ->
            Nothing


extractTuple2Maybe : ( Maybe a, Maybe b ) -> Maybe ( a, b )
extractTuple2Maybe ( ma, mb ) =
    case ( ma, mb ) of
        ( Just a, Just b ) ->
            Just ( a, b )

        _ ->
            Nothing


tuple3ToList : ( a, a, a ) -> List a
tuple3ToList ( a, b, c ) =
    [ a, b, c ]


listToTuple3 : List a -> Maybe ( a, a, a )
listToTuple3 list =
    ( List.Extra.getAt 0 list
    , List.Extra.getAt 1 list
    , List.Extra.getAt 2 list
    )
        |> extractTuple3Maybe


mapTuple2 : (a -> b) -> ( a, a ) -> ( b, b )
mapTuple2 f =
    Tuple.mapBoth f f


tuple3First : ( a, a, a ) -> a
tuple3First ( a, b, c ) =
    a


tuple3Middle : ( a, a, a ) -> a
tuple3Middle ( a, b, c ) =
    b


tuple3Last : ( a, a, a ) -> a
tuple3Last ( a, b, c ) =
    c
