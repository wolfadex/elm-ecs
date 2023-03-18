# elm-ecs

An ECS library for Elm. Provides an easy way to build a full game using a common Entity-Component-System architecture.

## Entity–component–system (ECS)

is an architectural pattern that is mostly used in game development. ECS follows the composition over inheritance principle that allows greater flexibility in defining entities where every object in a game's scene is an entity (e.g. enemies, bullets, vehicles, etc.). Every entity consists of one or more components that add behavior or functionality. Therefore, the behavior of an entity can be changed at runtime by adding or removing components. This eliminates the ambiguity problems of deep and wide inheritance hierarchies that are difficult to understand, maintain and extend. Common ECS approaches are highly compatible and often combined with data-oriented design techniques.

## Entity

An entity is a unique ID associated with a combination of components

```elm
import Ecs.Entity

Ecs.Entity.create ecsConfigSpec world
    |> Ecs.Entity.with ( positionSpec, ( 100, 100 ) )
    |> Ecs.Entity.with ( velocitySpec, ( -1, -1 ) )
```

## Component

A component can be any data

```elm
type alias Position =
    ( Int, Int )

type alias Velocity =
    ( Int, Int )

type alias Life =
    Int

type Pawn
    = Player
    | Enemy
```

## System

A system takes in 1 or more components and updates the state of those components

```elm
moveSystem : Ecs.Component.Spec Velocity world -> Ecs.Component.Spec Position world -> System world
moveSystem =
    Ecs.System.step2
        (\( velocity, _ ) ( position, setPosition ) ->
            setPosition (Vec2.add velocity position)
        )
```

## Example `Main.elm`

```elm
import Browser
import Browser.Events
import Ecs
import Ecs.Component
import Ecs.Config
import Ecs.Entity
import Ecs.System


{-| Defines our game world
-}
type alias World =
    { ecsConfig : Ecs.Config -- Used for Ecs internals
    , age : Ecs.Component Int -- Holds data related to the age of all entities
    }


{-| Used internally by Ecs for creating new entities
-}
ecsConfigSpec : Ecs.Config.Spec World
ecsConfigSpec =
    { get = .ecsConfig
    , set = \config world -> { world | ecsConfig = config }
    }


{-| Our initial world state
-}
baseWorld : World
baseWorld =
    { ecsConfig = Ecs.Config.init
    , ageComponent = Ecs.Component.empty
    }


ageSpec : Ecs.Component.Spec Int World
ageSpec =
    { get = .ageComponent
    , set = \ageComponent world -> { world | ageComponent = ageComponent }
    }


init : () -> ( World, Cmd Msg )
init () =
    let
        ( _, world ) =
            Ecs.Entity.create ecsConfigSpec baseWorld -- We first create an Entity
                |> Ecs.Entity.with ( ageSpec, 0 ) -- Then we add a component to it
    in
    ( world, Cmd.none )


type Msg
    = Tick


subscriptions : World -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame (\_ -> Tick)


ageSystem : Ecs.System.System World
ageSystem =
    Ecs.System.step (\age -> age + 1)


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        Tick ->
            ( world
                |> ageSystem -- Run the ageSystem
            , Cmd.none
            )


main : Program () World Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


view : World -> Html Msg
view world =
    ...


```

Notes:

This started as a fork of [justgook/elm-game-logic](https://package.elm-lang.org/packages/justgook/elm-game-logic/latest/), credit goes to them for the initial design. Notable changes:

- Uses `Dict` instead of `Array` internally
- `Entity`s (the ID) is a pair of ID and version to support reusing IDs
- `stepN` was renamed to `mapN`
- `Entity` creation is managed for you
