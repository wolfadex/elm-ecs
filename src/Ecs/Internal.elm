module Ecs.Internal exposing
    ( Component(..)
    , Config(..)
    , Entity(..)
    , update
    )

import Dict exposing (Dict)


type Entity
    = Entity ( Int, Int )


type Config
    = Config ( Int, List Entity )


type Component data
    = Component (Dict ( Int, Int ) data)


update : Entity -> (a -> a) -> Dict ( Int, Int ) a -> Dict ( Int, Int ) a
update (Entity id) f entities =
    case Dict.get id entities of
        Nothing ->
            entities

        Just data ->
            Dict.insert id (f data) entities
