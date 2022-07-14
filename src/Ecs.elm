module Ecs exposing
    ( EntityId
    , Component
    , Config
    )

{-|

@docs EntityId
@docs Component
@docs Config

-}

import Ecs.Internal


{-| The ID of an entity
-}
type alias EntityId =
    Ecs.Internal.EntityId


{-| Component storage, the main building block of the world
-}
type alias Component data =
    Ecs.Internal.Component data


{-| Data that is used for managing and operating the ECS internally
-}
type alias Config =
    Ecs.Internal.Config
