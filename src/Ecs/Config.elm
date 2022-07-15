module Ecs.Config exposing
    ( Spec
    , init
    )

{-|

@docs Spec
@docs init

-}

import Ecs exposing (Config)
import Ecs.Internal exposing (Config(..))
import Set


{-| Initializes your `Ecs` setup
-}
init : Config
init =
    Config ( 0, Set.empty )


{-| Similar to the `Ecs.Component.Spec` but for `Config`
-}
type alias Spec world =
    { get : world -> Config
    , set : Config -> world -> world
    }
