module Example exposing (suite)

import Dict
import Ecs
import Ecs.Component
import Ecs.Config
import Ecs.Entity
import Ecs.System
import Expect
import Test exposing (Test)


type alias World =
    { ecsConfig : Ecs.Config
    , age : Ecs.Component Int
    }


worldConfig : Ecs.Config.Spec World
worldConfig =
    { get = .ecsConfig
    , set = \config w -> { w | ecsConfig = config }
    }


ageSpec : Ecs.Component.Spec Int World
ageSpec =
    { get = .age
    , set = \age w -> { w | age = age }
    }


ageSystem : Ecs.System.System World
ageSystem =
    Ecs.System.step
        (\age -> age + 1)
        ageSpec


suite : Test
suite =
    -- Test.todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
    Test.describe "basic entity & component"
        [ Test.test "entity creation" <|
            \() ->
                let
                    initialWorld : World
                    initialWorld =
                        { ecsConfig = Ecs.Config.init
                        , age = Ecs.Component.empty
                        }

                    ( _, worldWithEntity ) =
                        Ecs.Entity.create worldConfig initialWorld
                            |> Ecs.Entity.with ( ageSpec, 0 )
                in
                worldWithEntity
                    |> .age
                    |> Ecs.Component.toDict
                    |> Expect.equalDicts (Dict.singleton ( 0, 0 ) 0)
        , Test.test "system updates component" <|
            \() ->
                let
                    initialWorld : World
                    initialWorld =
                        { ecsConfig = Ecs.Config.init
                        , age = Ecs.Component.empty
                        }

                    ( _, worldWithEntity ) =
                        Ecs.Entity.create worldConfig initialWorld
                            |> Ecs.Entity.with ( ageSpec, 0 )
                in
                worldWithEntity
                    |> ageSystem
                    |> ageSystem
                    |> .age
                    |> Ecs.Component.toDict
                    |> Expect.equalDicts (Dict.singleton ( 0, 0 ) 2)
        ]
