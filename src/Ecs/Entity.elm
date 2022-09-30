module Ecs.Entity exposing
    ( create
    , with
    , remove
    , delete
    )

{-|


# Creation

@docs create


# Build

@docs with
@docs remove
@docs delete

-}

import Dict
import Ecs exposing (Entity)
import Ecs.Component
import Ecs.Config
import Ecs.Internal exposing (Component(..), Config(..), Entity(..))


{-| Creates a new

    Ecs.Entity.create ecsConfigSpec world

-}
create : Ecs.Config.Spec world -> world -> ( Entity, world )
create config world =
    let
        (Config ( nextId, availableIds )) =
            config.get world
    in
    case availableIds of
        [] ->
            ( Entity ( nextId, 0 )
            , config.set (Config ( nextId + 1, availableIds )) world
            )

        (Entity ( id, version )) :: remainingIds ->
            ( Entity ( id, version + 1 )
            , config.set (Config ( nextId, remainingIds )) world
            )


{-| For creating `Entity` destruction functions, should pipe in all possible component specs.
It also can be used to remove some/select components from an entity.

    deleteEntity : ( EntityId, World ) -> ( EntityId, World )
    deleteEntity =
        Ecs.Entity.remove positionSpec
            >> Ecs.Entity.remove velocitySpec
            >> Ecs.Entity.delete ecsConfigSpec

    newWorld : World
    newWorld =
        deleteEntity ( id, world )

-}
remove : Ecs.Component.Spec comp world -> ( Entity, world ) -> ( Entity, world )
remove spec ( Entity id, world ) =
    ( Entity id
    , spec.set
        (let
            (Component comp) =
                spec.get world
         in
         Component (Dict.remove id comp)
        )
        world
    )


{-| Finalizes the deletion of an Entity from the world. This should be used after `remove`.
-}
delete : Ecs.Config.Spec world -> ( Entity, world ) -> ( Entity, world )
delete config ( Entity id, world ) =
    ( Entity id
    , config.set
        (let
            (Config ( nextId, availableIds )) =
                config.get world
         in
         Config ( nextId, Entity id :: availableIds )
        )
        world
    )


{-| Adds a component to an entity

    Ecs.Entity.create ecsConfigSpec world
        |> Ecs.Entity.with ( positionSpec, positionComponent )
        |> Ecs.Entity.with ( velocitySpec, velocityComponent )

-}
with : ( Ecs.Component.Spec comp world, comp ) -> ( Entity, world ) -> ( Entity, world )
with ( spec, component ) ( id, world ) =
    let
        updatedComponents : Component comp
        updatedComponents =
            Ecs.Component.set id
                component
                (spec.get world)

        updatedWorld : world
        updatedWorld =
            spec.set updatedComponents world
    in
    ( id, updatedWorld )
