module Logic.Entity exposing (EntityId, create, with, remove)

{-| **Entity**: The entity is a general-purpose object. It only consists of a unique ID. They "tag every coarse game object as a separate item".
Example:

    Entity.create id world
        |> Entity.with ( positionSpec, positionComponent )
        |> Entity.with ( velocitySpec, velocityComponent )

@docs EntityId, create, with, remove

-}

import Array
import Logic.Component as Component


{-| The ID of an entity
-}
type alias EntityId =
    Int


{-| Start point for spawning `Entity`

    Entity.create id world
        |> Entity.with ( positionSpec, positionComponent )
        |> Entity.with ( velocitySpec, velocityComponent )

-}
create : EntityId -> world -> ( EntityId, world )
create id world =
    ( id, world )


{-| Way to create `Entity` destruction functions, should pipe in all possible component specs.
It also can be used to just disable (remove) some components from an entity.

    remove =
        Entity.remove positionSpec
            >> Entity.remove velocitySpec

    newWorld =
        remove ( id, world )

-}
remove : Component.Spec comp world -> ( EntityId, world ) -> ( EntityId, world )
remove spec ( entityID, world ) =
    ( entityID, spec.set (Array.set entityID Nothing (spec.get world)) world )


{-| Set component to spawn with a new entity

    Entity.create ( id, world )
        |> Entity.with ( positionSpec, positionComponent )
        |> Entity.with ( velocitySpec, velocityComponent )

-}
with : ( Component.Spec comp world, comp ) -> ( EntityId, world ) -> ( EntityId, world )
with ( spec, component ) ( entityID, world ) =
    let
        updatedComponents : Component.Set comp
        updatedComponents =
            Component.spawn entityID
                component
                (spec.get world)

        updatedWorld : world
        updatedWorld =
            spec.set updatedComponents world
    in
    ( entityID, updatedWorld )
