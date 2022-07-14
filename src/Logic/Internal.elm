module Logic.Internal exposing (indexedFoldlArray, update)

import Array exposing (Array)


indexedFoldlArray : (Int -> a -> b -> b) -> b -> Array a -> b
indexedFoldlArray func acc list =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, thisAcc ) =
            ( i + 1, func i x thisAcc )
    in
    Tuple.second (Array.foldl step ( 0, acc ) list)


update : Int -> (a -> a) -> Array a -> Array a
update n f a =
    case Array.get n a of
        Nothing ->
            a

        Just element_ ->
            Array.set n (f element_) a
