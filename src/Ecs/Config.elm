module Ecs.Config exposing (Config, Spec, init)

import Ecs.Internal exposing (EcsConfig(..))
import Set


init : Config
init =
    EcsConfig ( 0, Set.empty )


type alias Config =
    Ecs.Internal.EcsConfig


type alias Spec world =
    { get : world -> Config
    , set : Config -> world -> world
    }
