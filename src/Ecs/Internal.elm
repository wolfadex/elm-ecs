module Ecs.Internal exposing
    ( Component(..)
    , Config(..)
    , Entity(..)
    , indexedFoldl
    , update
    )

import Dict exposing (Dict)


type Entity
    = Entity ( Int, Int )


type Config
    = Config ( Int, List Entity )


type Component data
    = Component (Dict ( Int, Int ) data)


indexedFoldl : (Entity -> a -> b -> b) -> b -> Dict ( Int, Int ) a -> b
indexedFoldl func acc entities =
    let
        step : ( Int, Int ) -> a -> b -> b
        step id a thisAcc =
            func (Entity id) a thisAcc
    in
    Dict.foldl step acc entities


update : Entity -> (a -> a) -> Dict ( Int, Int ) a -> Dict ( Int, Int ) a
update (Entity id) f entities =
    case Dict.get id entities of
        Nothing ->
            entities

        Just data ->
            Dict.insert id (f data) entities
