module Ecs.Component exposing
    ( Component, Spec, empty
    , set, spawn, remove
    , get, get2, update
    , map, filterMap
    , fromList, toList
    , fromDict, toDict
    )

{-| **Component**: the raw data for one aspect of the object, and how it interacts with the world. "Labels the Entity as possessing this particular aspect".

Example:

    import Ecs.Component

    type alias Velocity =
        { x : Float, y : Float }

    spec : Ecs.Component.Spec Velocity { world | v : Ecs.Component.Component Velocity }
    spec =
        Ecs.Component.Spec .v (\comps world -> { world | v = comps })

    empty : Ecs.Component.Component Velocity
    empty =
        Ecs.Component.empty

@docs Component, Spec, empty


# Manipulations

@docs set, spawn, remove
@docs get, get2, update
@docs map, filterMap


# List

@docs fromList, toList


# Dict

@docs fromDict, toDict

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Ecs.Internal exposing (EntityId(..))


{-| The ID of an entity
-}
type alias EntityId =
    Ecs.Internal.EntityId


{-| Component storage, the main building block of the world
-}
type alias Component comp =
    Array (Maybe comp)


{-| Component specification, how to get a `Component` from the world and set back into the world (mainly used by Systems)
-}
type alias Spec comp world =
    { get : world -> Component comp
    , set : Component comp -> world -> world
    }


{-| Create an empty `Component` - mostly used to init component sets in the world.
-}
empty : Component comp
empty =
    Array.empty


{-| Remove `Component` from an entity by `EntityId`, or return unchanged if the entity never has a `Component`.
-}
remove : EntityId -> Component a -> Component a
remove (EntityId entityId) components =
    Array.set entityId Nothing components


{-| Safe way to create a component, same as `set`, only if an index is out of range `Component` will be stretched.

    test =
        -- Just 5
        empty |> spawn 5 10 |> get 5

-}
spawn : EntityId -> a -> Component a -> Component a
spawn (EntityId entityId) value components =
    if entityId - Array.length components < 0 then
        Array.set entityId (Just value) components

    else
        Array.push (Just value)
            (Array.append components
                (Array.repeat (entityId - Array.length components) Nothing)
            )


{-| Set the component at a particular index. Returns an updated `Component`. If the index is out of range, the `Component` is unaltered.

    test =
        -- Nothing
        empty |> set 5 10 |> get 5

-}
set : EntityId -> a -> Component a -> Component a
set (EntityId entityId) value components =
    Array.set entityId (Just value) components


{-| Get component for `EntityId`.
-}
get : EntityId -> Component comp -> Maybe comp
get (EntityId entityId) =
    Array.get entityId >> Maybe.withDefault Nothing


{-| Get components Tuple for `EntityId`.
-}
get2 : EntityId -> Component comp -> Component comp2 -> Maybe ( comp, comp2 )
get2 entityId set1 set2 =
    Maybe.map2 Tuple.pair
        (get entityId set1)
        (get entityId set2)


{-| Filter out certain components.
-}
filterMap : (comp -> Maybe comp) -> Component comp -> Component comp
filterMap f comps =
    Array.map (Maybe.andThen f) comps


{-| Apply a function on every component in a `Component`.

    map sqrt (fromList [ ( 0, 1 ), ( 1, 4 ), ( 2, 9 ) ])
        |> (==) fromList [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ) ]

-}
map : (comp -> comp) -> Component comp -> Component comp
map f comps =
    Array.map (Maybe.map f) comps


{-| Update Component by `EntityId`.
-}
update : EntityId -> (comp -> comp) -> Component comp -> Component comp
update entityId f =
    Ecs.Internal.update entityId (Maybe.map f)


{-| Create a `Component` from a `List`.

**Note**: Useful for data serialization.

-}
fromList : List ( EntityId, a ) -> Component a
fromList =
    List.foldl (\( index, value ) components -> spawn index value components) empty


{-| Convert a `Component` into an association list of id-component pairs, sorted by id.

**Note**: Useful for data deserialization.

-}
toList : Component a -> List ( EntityId, a )
toList =
    Ecs.Internal.indexedFoldlArray
        (\i a acc ->
            case a of
                Nothing ->
                    acc

                Just a_ ->
                    ( i, a_ ) :: acc
        )
        []


{-| Create a `Component` from a dictionary.

**Note**: Useful for data serialization.

-}
fromDict : Dict Int a -> Component a
fromDict =
    Dict.foldl (\index value components -> spawn (EntityId index) value components) empty


{-| Create a dictionary from a `Component`.

**Note**: Useful for data deserialization.

-}
toDict : Component a -> Dict Int a
toDict =
    Ecs.Internal.indexedFoldlArray
        (\(EntityId entityId) a acc ->
            case a of
                Nothing ->
                    acc

                Just a_ ->
                    Dict.insert entityId a_ acc
        )
        Dict.empty
