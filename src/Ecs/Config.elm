module Ecs.Config exposing
    ( Config
    , Spec
    , init
    )

{-|

@docs Config
@docs Spec
@docs init

-}

import Ecs.Internal exposing (EcsConfig(..))
import Set


{-| Initializes your ECS setup
-}
init : Config
init =
    EcsConfig ( 0, Set.empty )


{-| Data that is used for managing and operating the ECS internally
-}
type alias Config =
    Ecs.Internal.EcsConfig


{-| Similar to the `Ecs.Component.Spec` but for your [`Config`](#Config)
-}
type alias Spec world =
    { get : world -> Config
    , set : Config -> world -> world
    }
