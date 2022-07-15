module Ecs.Component exposing
    ( Spec
    , empty, set, update, remove
    , get, get2
    , map, filter, indexedFilter
    , fromList, toList
    , fromDict, toDict
    )

{-|

    import Ecs.Component

    type alias Velocity =
        { x : Float, y : Float }

    velocitySpec : Ecs.Component.Spec Velocity { world | velocityComponent : Ecs.Component Velocity }
    velocitySpec =
        { get = .velocityComponent
        , set = \component world -> { world | velocityComponent = component }
        }

    initVelocity : Ecs.Component.Component Velocity
    initVelocity =
        Ecs.Component.empty


# Create

@docs Spec


# Build

@docs empty, set, update, remove


# Query

@docs get, get2


# Transform

@docs map, filter, indexedFilter


# List

@docs fromList, toList


# Dict

@docs fromDict, toDict

-}

import Array
import Dict exposing (Dict)
import Ecs exposing (Component, EntityId)
import Ecs.Internal exposing (Component(..), EntityId(..))


{-| Component specification, how to get a `Component` from the world and set it back into the world. Used when creating new entities and when running systems.
-}
type alias Spec comp world =
    { get : world -> Component comp
    , set : Component comp -> world -> world
    }


{-| Create an empty `Component` - mostly used to init components in the world.
-}
empty : Component comp
empty =
    Component Array.empty


{-| Remove `Component` from an entity by `EntityId`, or return unchanged if the entity never had a `Component`.
-}
remove : EntityId -> Component a -> Component a
remove (EntityId entityId) (Component components) =
    Component (Array.set entityId Nothing components)


{-| Set the value of a component for an `EntityId`.
-}
set : EntityId -> a -> Component a -> Component a
set (EntityId entityId) value (Component components) =
    Component
        (if entityId - Array.length components < 0 then
            Array.set entityId (Just value) components

         else
            Array.push (Just value)
                (Array.append components
                    (Array.repeat (entityId - Array.length components) Nothing)
                )
        )


{-| Get component for `EntityId`.
-}
get : EntityId -> Component comp -> Maybe comp
get (EntityId entityId) (Component component) =
    case Array.get entityId component of
        Nothing ->
            Nothing

        Just a ->
            a


{-| Get components Tuple for `EntityId`.
-}
get2 : EntityId -> Component comp -> Component comp2 -> Maybe ( comp, comp2 )
get2 entityId set1 set2 =
    Maybe.map2 Tuple.pair
        (get entityId set1)
        (get entityId set2)


{-| Removes a component from an Entity
-}
filter : (comp -> Maybe comp) -> Component comp -> Component comp
filter f (Component comps) =
    Component (Array.map (Maybe.andThen f) comps)


{-| Removes a component from an Entity
-}
indexedFilter : (EntityId -> comp -> Maybe comp) -> Component comp -> Component comp
indexedFilter f (Component comps) =
    Component (Array.indexedMap (\id -> Maybe.andThen (f (EntityId id))) comps)


{-| Apply a function on every entity with a `Component`.

    Ecs.Component.fromList [ ( 0, 1 ), ( 1, 14 ), ( 2, 89 ) ]
        |> Ecs.Component.map (\age -> age + 1)
        |> (==) (Ecs.Component.fromList [ ( 0, 2 ), ( 1, 15 ), ( 2, 90 ) ])

-}
map : (comp -> comp) -> Component comp -> Component comp
map f (Component comps) =
    Component (Array.map (Maybe.map f) comps)


{-| Update Component by `EntityId`.
-}
update : EntityId -> (comp -> comp) -> Component comp -> Component comp
update entityId f (Component comp) =
    Component (Ecs.Internal.update entityId (Maybe.map f) comp)


{-| Create a `Component` from a `List`.

**Note**: Useful for data serialization.

-}
fromList : List ( EntityId, a ) -> Component a
fromList =
    List.foldl (\( index, value ) components -> set index value components) empty


{-| Convert a `Component` into an association list of id-component pairs, sorted by id.

**Note**: Useful for data deserialization.

-}
toList : Component a -> List ( EntityId, a )
toList (Component comp) =
    Ecs.Internal.indexedFoldlArray
        (\i a acc ->
            case a of
                Nothing ->
                    acc

                Just a_ ->
                    ( i, a_ ) :: acc
        )
        []
        comp


{-| Create a `Component` from a `Dict`.

**Note**: Useful for data serialization.

-}
fromDict : Dict Int a -> Component a
fromDict =
    Dict.foldl (\index value components -> set (EntityId index) value components) empty


{-| Create a `Dict` from a `Component`.

**Note**: Useful for data deserialization.

-}
toDict : Component a -> Dict Int a
toDict (Component comp) =
    Ecs.Internal.indexedFoldlArray
        (\(EntityId entityId) a acc ->
            case a of
                Nothing ->
                    acc

                Just a_ ->
                    Dict.insert entityId a_ acc
        )
        Dict.empty
        comp
