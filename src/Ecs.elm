module Ecs exposing
    ( Entity
    , Component
    , Config
    )

{-|

@docs Entity
@docs Component
@docs Config

-}

import Ecs.Internal


{-| The ID of an entity.
-}
type alias Entity =
    Ecs.Internal.Entity


{-| The raw data for one aspect of an `Entity`. I.e. labels the Entity as possessing this particular aspect.
-}
type alias Component data =
    Ecs.Internal.Component data


{-| Data that is used for managing and operating the ECS internally
-}
type alias Config =
    Ecs.Internal.Config
