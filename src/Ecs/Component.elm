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

import Dict exposing (Dict)
import Ecs exposing (Component, Entity)
import Ecs.Internal exposing (Component(..), Entity(..))


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
    Component Dict.empty


{-| Remove `Component` from an entity by `Entity`, or return unchanged if the entity never had a `Component`.
-}
remove : Entity -> Component a -> Component a
remove (Entity id) (Component components) =
    Component (Dict.remove id components)


{-| Set the value of a component for an `Entity`.
-}
set : Entity -> a -> Component a -> Component a
set (Entity id) value (Component components) =
    Component (Dict.insert id value components)


{-| Get component for `Entity`.
-}
get : Entity -> Component comp -> Maybe comp
get (Entity id) (Component component) =
    Dict.get id component


{-| Get components Tuple for `Entity`.
-}
get2 : Entity -> Component comp -> Component comp2 -> Maybe ( comp, comp2 )
get2 entity set1 set2 =
    Maybe.map2 Tuple.pair
        (get entity set1)
        (get entity set2)


{-| Removes a component from an Entity
-}
filter : (comp -> Bool) -> Component comp -> Component comp
filter f (Component comps) =
    Component
        (Dict.filter
            (\_ comp -> f comp)
            comps
        )


{-| Removes a component from an Entity
-}
indexedFilter : (Entity -> comp -> Bool) -> Component comp -> Component comp
indexedFilter f (Component comps) =
    Component
        (Dict.filter
            (\id comp -> f (Entity id) comp)
            comps
        )


{-| Apply a function on every entity with a `Component`.

    Ecs.Component.fromList [ ( 0, 1 ), ( 1, 14 ), ( 2, 89 ) ]
        |> Ecs.Component.map (\age -> age + 1)
        |> (==) (Ecs.Component.fromList [ ( 0, 2 ), ( 1, 15 ), ( 2, 90 ) ])

-}
map : (comp -> comp) -> Component comp -> Component comp
map f (Component comps) =
    Component (Dict.map (\_ comp -> f comp) comps)


{-| Update Component by `Entity`.
-}
update : Entity -> (comp -> comp) -> Component comp -> Component comp
update entity f (Component comp) =
    Component (Ecs.Internal.update entity f comp)


{-| Create a `Component` from a `List`.

**Note**: Useful for data serialization.

-}
fromList : List ( ( Int, Int ), a ) -> Component a
fromList list =
    list
        |> Dict.fromList
        |> Component


{-| Convert a `Component` into an association list of id-component pairs, sorted by id.

**Note**: Useful for data deserialization.

-}
toList : Component a -> List ( ( Int, Int ), a )
toList (Component comp) =
    Dict.toList comp


{-| Create a `Component` from a `Dict`.

**Note**: Useful for data serialization.

-}
fromDict : Dict ( Int, Int ) a -> Component a
fromDict =
    Component


{-| Create a `Dict` from a `Component`.

**Note**: Useful for data deserialization.

-}
toDict : Component a -> Dict ( Int, Int ) a
toDict (Component comp) =
    comp
