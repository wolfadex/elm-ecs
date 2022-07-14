module Ecs.Entity exposing (spawn, with, remove)

{-| **Entity**: The entity is a general-purpose object. It only consists of a unique ID. They "tag every coarse game object as a separate item".
Example:

    import Ecs.Entity

    Ecs.Entity.spawn ecsConfigSpec world
        |> Ecs.Entity.with ( positionSpec, positionComponent )
        |> Ecs.Entity.with ( velocitySpec, velocityComponent )

@docs spawn, with, remove

-}

import Array
import Ecs exposing (Component, EntityId)
import Ecs.Component
import Ecs.Config
import Ecs.Internal exposing (Component(..), Config(..), EntityId(..))
import Set


{-| Start point for spawning an `Entity`

    Ecs.Entity.spawn ecsConfigSpec world
        |> Ecs.Entity.with ( positionSpec, positionComponent )
        |> Ecs.Entity.with ( velocitySpec, velocityComponent )

-}
spawn : Ecs.Config.Spec world -> world -> ( EntityId, world )
spawn config world =
    let
        (Config ( nextId, availableIds )) =
            config.get world
    in
    case Set.toList availableIds of
        [] ->
            ( EntityId nextId
            , config.set (Config ( nextId + 1, availableIds )) world
            )

        reuseableId :: _ ->
            ( EntityId nextId
            , config.set (Config ( reuseableId, Set.remove reuseableId availableIds )) world
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
    ( id
    , spec.set
        (Component
            (let
                (Component comp) =
                    spec.get world
             in
             Array.set entityId Nothing comp
            )
        )
        world
    )


{-| Set component to spawn with a new entity

    Ecs.Entity.spawn ecsConfigSpec world
        |> Ecs.Entity.with ( positionSpec, positionComponent )
        |> Ecs.Entity.with ( velocitySpec, velocityComponent )

-}
with : ( Ecs.Component.Spec comp world, comp ) -> ( EntityId, world ) -> ( EntityId, world )
with ( spec, component ) ( entityID, world ) =
    let
        updatedComponents : Component comp
        updatedComponents =
            Ecs.Component.spawn entityID
                component
                (spec.get world)

        updatedWorld : world
        updatedWorld =
            spec.set updatedComponents world
    in
    ( entityID, updatedWorld )
