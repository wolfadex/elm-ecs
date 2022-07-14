module Ecs.Internal exposing
    ( Component(..)
    , Config(..)
    , EntityId(..)
    , indexedFoldlArray
    , update
    )

import Array exposing (Array)
import Set exposing (Set)


type EntityId
    = EntityId Int


type Config
    = Config ( Int, Set Int )


type Component data
    = Component (Array (Maybe data))


indexedFoldlArray : (EntityId -> a -> b -> b) -> b -> Array a -> b
indexedFoldlArray func acc list =
    let
        step : a -> ( EntityId, b ) -> ( EntityId, b )
        step x ( (EntityId i) as id, thisAcc ) =
            ( EntityId (i + 1), func id x thisAcc )
    in
    Tuple.second (Array.foldl step ( EntityId 0, acc ) list)


update : EntityId -> (a -> a) -> Array a -> Array a
update (EntityId n) f a =
    case Array.get n a of
        Nothing ->
            a

        Just element_ ->
            Array.set n (f element_) a
