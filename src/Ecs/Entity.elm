module Ecs.Entity exposing
    ( EntityId, with, remove
    , EcsConfig, EcsConfigSpec, initConfig, spawn
    )

{-| **Entity**: The entity is a general-purpose object. It only consists of a unique ID. They "tag every coarse game object as a separate item".
Example:

    import Ecs.Entity

    Ecs.Entity.create id world
        |> Ecs.Entity.with ( positionSpec, positionComponent )
        |> Ecs.Entity.with ( velocitySpec, velocityComponent )

@docs EntityId, create, with, remove

-}

import Array
import Ecs.Component
import Ecs.Internal exposing (EntityId(..))
import Set exposing (Set)


initConfig : EcsConfig
initConfig =
    EcsConfig ( 0, Set.empty )


type EcsConfig
    = EcsConfig ( Int, Set Int )


type alias EcsConfigSpec world =
    { get : world -> EcsConfig
    , set : EcsConfig -> world -> world
    }


{-| The ID of an entity
-}
type alias EntityId =
    Ecs.Internal.EntityId


{-| Start point for spawning an `Entity`

    Ecs.Entity.create id world
        |> Ecs.Entity.with ( positionSpec, positionComponent )
        |> Ecs.Entity.with ( velocitySpec, velocityComponent )

-}
spawn : EcsConfigSpec world -> world -> ( EntityId, world )
spawn config world =
    let
        (EcsConfig ( nextId, availableIds )) =
            config.get world
    in
    case Set.toList availableIds of
        [] ->
            ( EntityId nextId
            , config.set (EcsConfig ( nextId + 1, availableIds )) world
            )

        reuseableId :: _ ->
            ( EntityId nextId
            , config.set (EcsConfig ( reuseableId, Set.remove reuseableId availableIds )) world
            )


{-| Way to create `Entity` destruction functions, should pipe in all possible component specs.
It also can be used to just disable (remove) some components from an entity.

    remove =
        Ecs.Entity.remove positionSpec
            >> Ecs.Entity.remove velocitySpec

    newWorld =
        remove ( id, world )

-}
remove : Ecs.Component.Spec comp world -> ( EntityId, world ) -> ( EntityId, world )
remove spec ( (EntityId entityId) as id, world ) =
    ( id, spec.set (Array.set entityId Nothing (spec.get world)) world )


{-| Set component to spawn with a new entity

    Ecs.Entity.create ( id, world )
        |> Ecs.Entity.with ( positionSpec, positionComponent )
        |> Ecs.Entity.with ( velocitySpec, velocityComponent )

-}
with : ( Ecs.Component.Spec comp world, comp ) -> ( EntityId, world ) -> ( EntityId, world )
with ( spec, component ) ( entityID, world ) =
    let
        updatedComponents : Ecs.Component.Set comp
        updatedComponents =
            Ecs.Component.spawn entityID
                component
                (spec.get world)

        updatedWorld : world
        updatedWorld =
            spec.set updatedComponents world
    in
    ( entityID, updatedWorld )
