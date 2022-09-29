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


{-| Initializes your `Ecs` setup
-}
init : Config
init =
    Config ( 0, [] )


{-| Similar to the `Ecs.Component.Spec` but for `Config`
-}
type alias Spec world =
    { get : world -> Config
    , set : Config -> world -> world
    }
