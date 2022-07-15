module Ecs.Entity exposing
    ( create
    , with
    , remove
    )

{-|


# Creation

@docs create


# Build

@docs with
@docs remove

-}

import Array
import Ecs exposing (Component, EntityId)
import Ecs.Component
import Ecs.Config
import Ecs.Internal exposing (Component(..), Config(..), EntityId(..))
import Set


{-| Creates a new

    Ecs.Entity.create ecsConfigSpec world

-}
create : Ecs.Config.Spec world -> world -> ( EntityId, world )
create config world =
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


{-| For creating `Entity` destruction functions, should pipe in all possible component specs.
It also can be used to remove some/select components from an entity.

    deleteEntity : ( EntityId, World ) -> ( EntityId, World )
    deleteEntity =
        Ecs.Entity.remove positionSpec
            >> Ecs.Entity.remove velocitySpec

    newWorld : World
    newWorld =
        deleteEntity ( id, world )

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


{-| Adds a component to an entity

    Ecs.Entity.create ecsConfigSpec world
        |> Ecs.Entity.with ( positionSpec, positionComponent )
        |> Ecs.Entity.with ( velocitySpec, velocityComponent )

-}
with : ( Ecs.Component.Spec comp world, comp ) -> ( EntityId, world ) -> ( EntityId, world )
with ( spec, component ) ( entityID, world ) =
    let
        updatedComponents : Component comp
        updatedComponents =
            Ecs.Component.set entityID
                component
                (spec.get world)

        updatedWorld : world
        updatedWorld =
            spec.set updatedComponents world
    in
    ( entityID, updatedWorld )
