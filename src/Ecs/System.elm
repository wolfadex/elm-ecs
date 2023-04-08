module Ecs.System exposing
    ( System
    , update, map, map2, map3, map4, map5
    , foldl, foldl2, foldl3, foldl4, foldl5
    , indexedFoldl, indexedFoldl2, indexedFoldl3, indexedFoldl4, indexedFoldl5
    , applyIf, applyMaybe
    , Acc2, Acc3, Acc4, Acc5
    )

{-|

@docs System
@docs update, map, map2, map3, map4, map5

@docs foldl, foldl2, foldl3, foldl4, foldl5
@docs indexedFoldl, indexedFoldl2, indexedFoldl3, indexedFoldl4, indexedFoldl5


# Util

@docs applyIf, applyMaybe


# Internal helper types

@docs Acc2, Acc3, Acc4, Acc5

-}

import Dict
import Ecs exposing (Entity)
import Ecs.Component
import Ecs.Internal exposing (Component(..), Entity(..))


{-| Update whole `Ecs.Component`
-}
update : Ecs.Component.Spec comp world -> (Ecs.Component comp -> Ecs.Component comp) -> System world
update spec f world =
    spec.set (f (spec.get world)) world


{-| A function for updating a `world`
-}
type alias System world =
    world -> world


{-| Reduce an `Ecs.Component` from the left.

Example count how many enemies are left in the world:

    enemyComponent : Ecs.Component.Component PlayerType
    enemyComponent =
        enemySpec.get world

    remainingEnemyCount : Int
    remainingEnemyCount =
        Ecs.System.foldl (\_ -> (+) 1) enemyComponent 0

-}
foldl : (comp -> acc -> acc) -> Ecs.Component comp -> acc -> acc
foldl f (Component comp) acc_ =
    Dict.foldl (\_ a acc -> f a acc)
        acc_
        comp


{-| Variant of `foldl` that passes the `Entity` of the current element to the map function.

`indexedFoldl` is to `foldl` as `List.indexedMap` is to `List.map`.

-}
indexedFoldl : (Entity -> comp -> acc -> acc) -> Ecs.Component comp -> acc -> acc
indexedFoldl f (Component comp) acc_ =
    Dict.foldl (\id a thisAcc -> f (Entity id) a thisAcc)
        acc_
        comp


{-| Map over all entities that have both components and reduce the `Component`s from the left.
-}
foldl2 : (comp1 -> comp2 -> acc -> acc) -> Ecs.Component comp1 -> Ecs.Component comp2 -> acc -> acc
foldl2 f comp1 comp2 acc_ =
    indexedFoldl
        (\entity a acc ->
            Maybe.map (\b -> f a b acc)
                (Ecs.Component.get entity comp2)
                |> Maybe.withDefault acc
        )
        comp1
        acc_


{-| Same as [`indexedFoldl`](#indexedFoldl) only with 2 components
-}
indexedFoldl2 : (Entity -> comp1 -> comp2 -> acc -> acc) -> Ecs.Component comp1 -> Ecs.Component comp2 -> acc -> acc
indexedFoldl2 f comp1 comp2 acc_ =
    indexedFoldl
        (\entity a acc ->
            Maybe.map (\b -> f entity a b acc)
                (Ecs.Component.get entity comp2)
                |> Maybe.withDefault acc
        )
        comp1
        acc_


{-| Same as [`foldl2`](#foldl2) only with 3 components
-}
foldl3 : (comp1 -> comp2 -> comp3 -> acc -> acc) -> Ecs.Component comp1 -> Ecs.Component comp2 -> Ecs.Component comp3 -> acc -> acc
foldl3 f comp1 comp2 comp3 acc_ =
    indexedFoldl
        (\n a acc ->
            Maybe.map2 (\b c -> f a b c acc)
                (Ecs.Component.get n comp2)
                (Ecs.Component.get n comp3)
                |> Maybe.withDefault acc
        )
        comp1
        acc_


{-| Same as [`indexedFoldl2`](#indexedFoldl2) only with 3 components
-}
indexedFoldl3 : (Entity -> comp1 -> comp2 -> comp3 -> acc -> acc) -> Ecs.Component comp1 -> Ecs.Component comp2 -> Ecs.Component comp3 -> acc -> acc
indexedFoldl3 f comp1 comp2 comp3 acc_ =
    indexedFoldl
        (\n a acc ->
            Maybe.map2 (\b c -> f n a b c acc)
                (Ecs.Component.get n comp2)
                (Ecs.Component.get n comp3)
                |> Maybe.withDefault acc
        )
        comp1
        acc_


{-| Same as [`foldl2`](#foldl2) only with 4 components
-}
foldl4 :
    (comp1 -> comp2 -> comp3 -> comp4 -> acc -> acc)
    -> Ecs.Component comp1
    -> Ecs.Component comp2
    -> Ecs.Component comp3
    -> Ecs.Component comp4
    -> acc
    -> acc
foldl4 f comp1 comp2 comp3 comp4 acc_ =
    indexedFoldl
        (\n a acc ->
            Maybe.map3 (\b c d -> f a b c d acc)
                (Ecs.Component.get n comp2)
                (Ecs.Component.get n comp3)
                (Ecs.Component.get n comp4)
                |> Maybe.withDefault acc
        )
        comp1
        acc_


{-| Same as [`indexedFoldl2`](#indexedFoldl2) only with 4 components
-}
indexedFoldl4 :
    (Entity -> comp1 -> comp2 -> comp3 -> comp4 -> acc -> acc)
    -> Ecs.Component comp1
    -> Ecs.Component comp2
    -> Ecs.Component comp3
    -> Ecs.Component comp4
    -> acc
    -> acc
indexedFoldl4 f comp1 comp2 comp3 comp4 acc_ =
    indexedFoldl
        (\n a acc ->
            Maybe.map3 (\b c d -> f n a b c d acc)
                (Ecs.Component.get n comp2)
                (Ecs.Component.get n comp3)
                (Ecs.Component.get n comp4)
                |> Maybe.withDefault acc
        )
        comp1
        acc_


{-| Same as [`foldl2`](#foldl2) only with 5 components
-}
foldl5 :
    (comp1 -> comp2 -> comp3 -> comp4 -> comp5 -> acc -> acc)
    -> Ecs.Component comp1
    -> Ecs.Component comp2
    -> Ecs.Component comp3
    -> Ecs.Component comp4
    -> Ecs.Component comp5
    -> acc
    -> acc
foldl5 f comp1 comp2 comp3 comp4 comp5 acc_ =
    indexedFoldl
        (\n a acc ->
            Maybe.map4 (\b c d e -> f a b c d e acc)
                (Ecs.Component.get n comp2)
                (Ecs.Component.get n comp3)
                (Ecs.Component.get n comp4)
                (Ecs.Component.get n comp5)
                |> Maybe.withDefault acc
        )
        comp1
        acc_


{-| Same as [`indexedFoldl2`](#indexedFoldl2) only with 5 components
-}
indexedFoldl5 :
    (Entity -> comp1 -> comp2 -> comp3 -> comp4 -> comp5 -> acc -> acc)
    -> Ecs.Component comp1
    -> Ecs.Component comp2
    -> Ecs.Component comp3
    -> Ecs.Component comp4
    -> Ecs.Component comp5
    -> acc
    -> acc
indexedFoldl5 f comp1 comp2 comp3 comp4 comp5 acc_ =
    indexedFoldl
        (\n a acc ->
            Maybe.map4 (\b c d e -> f n a b c d e acc)
                (Ecs.Component.get n comp2)
                (Ecs.Component.get n comp3)
                (Ecs.Component.get n comp4)
                (Ecs.Component.get n comp5)
                |> Maybe.withDefault acc
        )
        comp1
        acc_


{-| Single component mapping, similar to `List.map` - only for `Ecs.Component.Component` inside `world`

    gravitySystem : Ecs.System.System world
    gravitySystem =
        Logic.System.map (Vec2.add gravity) accelerationSpec

-}
map : (comp -> comp) -> Ecs.Component.Spec comp world -> System world
map f { get, set } world =
    let
        (Component comp) =
            get world
    in
    set (Component (Dict.map (\_ c -> f c) comp)) world


{-| Helper for [`map2`](#map2)
-}
type alias Acc2 a b =
    { a : Ecs.Component a
    , b : Ecs.Component b
    }


{-| Map over all entities that have both components.

Example:

    moveSystem : Ecs.System.System World
    moveSystem =
        Logic.System.map2
            (\( velocity, _ ) ( position, setPosition ) ->
                setPosition (Vec2.add velocity position)
            )
            velocitySpec
            positionSpec

-}
map2 :
    (( a, a -> System (Acc2 a b) )
     -> ( b, b -> System (Acc2 a b) )
     -> System (Acc2 a b)
    )
    -> Ecs.Component.Spec a world
    -> Ecs.Component.Spec b world
    -> System world
map2 f spec1 spec2 world =
    let
        set1 : Entity -> a -> System (Acc2 a b)
        set1 (Entity i) a acc =
            let
                (Component comp) =
                    acc.a
            in
            { acc | a = Component (Dict.insert i a comp) }

        set2 : Entity -> b -> System (Acc2 a b)
        set2 (Entity i) b acc =
            let
                (Component comp) =
                    acc.b
            in
            { acc | b = Component (Dict.insert i b comp) }

        combined : { a : Component a, b : Component b }
        combined =
            { a = spec1.get world
            , b = spec2.get world
            }

        result : Acc2 a b
        result =
            indexedFoldl
                (\n a acc ->
                    Maybe.map (\b -> f ( a, set1 n ) ( b, set2 n ) acc)
                        (Ecs.Component.get n acc.b)
                        |> Maybe.withDefault acc
                )
                combined.a
                combined
    in
    world
        |> applyIf (result.a /= combined.a) (spec1.set result.a)
        |> applyIf (result.b /= combined.b) (spec2.set result.b)


{-| Helper for [`map3`](#map3)
-}
type alias Acc3 a b c =
    { a : Ecs.Component a
    , b : Ecs.Component b
    , c : Ecs.Component c
    }


{-| Same as [`map2`](#map2) only with 3 components
-}
map3 :
    (( a, a -> System (Acc3 a b c) )
     -> ( b, b -> System (Acc3 a b c) )
     -> ( c, c -> System (Acc3 a b c) )
     -> System (Acc3 a b c)
    )
    -> Ecs.Component.Spec a world
    -> Ecs.Component.Spec b world
    -> Ecs.Component.Spec c world
    -> System world
map3 f spec1 spec2 spec3 world =
    let
        set1 : Entity -> a -> System (Acc3 a b c)
        set1 (Entity i) a acc =
            let
                (Component comp) =
                    acc.a
            in
            { acc | a = Component (Dict.insert i a comp) }

        set2 : Entity -> b -> System (Acc3 a b c)
        set2 (Entity i) b acc =
            let
                (Component comp) =
                    acc.b
            in
            { acc | b = Component (Dict.insert i b comp) }

        set3 : Entity -> c -> System (Acc3 a b c)
        set3 (Entity i) c acc =
            let
                (Component comp) =
                    acc.c
            in
            { acc | c = Component (Dict.insert i c comp) }

        combined : { a : Component a, b : Component b, c : Component c }
        combined =
            { a = spec1.get world
            , b = spec2.get world
            , c = spec3.get world
            }

        result : Acc3 a b c
        result =
            indexedFoldl
                (\n a acc ->
                    Maybe.map2
                        (\b c -> f ( a, set1 n ) ( b, set2 n ) ( c, set3 n ) acc)
                        (Ecs.Component.get n acc.b)
                        (Ecs.Component.get n acc.c)
                        |> Maybe.withDefault acc
                )
                combined.a
                combined
    in
    world
        |> applyIf (result.a /= combined.a) (spec1.set result.a)
        |> applyIf (result.b /= combined.b) (spec2.set result.b)
        |> applyIf (result.c /= combined.c) (spec3.set result.c)


{-| Helper for [`map4`](#map4)
-}
type alias Acc4 a b c d =
    { a : Ecs.Component a
    , b : Ecs.Component b
    , c : Ecs.Component c
    , d : Ecs.Component d
    }


{-| Same as [`map2`](#map2) only with 4 components
-}
map4 :
    (( a, a -> System (Acc4 a b c d) )
     -> ( b, b -> System (Acc4 a b c d) )
     -> ( c, c -> System (Acc4 a b c d) )
     -> ( d, d -> System (Acc4 a b c d) )
     -> System (Acc4 a b c d)
    )
    -> Ecs.Component.Spec a world
    -> Ecs.Component.Spec b world
    -> Ecs.Component.Spec c world
    -> Ecs.Component.Spec d world
    -> System world
map4 f spec1 spec2 spec3 spec4 world =
    let
        set1 : Entity -> a -> System (Acc4 a b c d)
        set1 (Entity i) a acc =
            let
                (Component comp) =
                    acc.a
            in
            { acc | a = Component (Dict.insert i a comp) }

        set2 : Entity -> b -> System (Acc4 a b c d)
        set2 (Entity i) b acc =
            let
                (Component comp) =
                    acc.b
            in
            { acc | b = Component (Dict.insert i b comp) }

        set3 : Entity -> c -> System (Acc4 a b c d)
        set3 (Entity i) c acc =
            let
                (Component comp) =
                    acc.c
            in
            { acc | c = Component (Dict.insert i c comp) }

        set4 : Entity -> d -> System (Acc4 a b c d)
        set4 (Entity i) d acc =
            let
                (Component comp) =
                    acc.d
            in
            { acc | d = Component (Dict.insert i d comp) }

        combined : { a : Component a, b : Component b, c : Component c, d : Component d }
        combined =
            { a = spec1.get world
            , b = spec2.get world
            , c = spec3.get world
            , d = spec4.get world
            }

        result : Acc4 a b c d
        result =
            indexedFoldl
                (\n a acc ->
                    Maybe.map3
                        (\b c d -> f ( a, set1 n ) ( b, set2 n ) ( c, set3 n ) ( d, set4 n ) acc)
                        (Ecs.Component.get n acc.b)
                        (Ecs.Component.get n acc.c)
                        (Ecs.Component.get n acc.d)
                        |> Maybe.withDefault acc
                )
                combined.a
                combined
    in
    world
        |> applyIf (result.a /= combined.a) (spec1.set result.a)
        |> applyIf (result.b /= combined.b) (spec2.set result.b)
        |> applyIf (result.c /= combined.c) (spec3.set result.c)
        |> applyIf (result.d /= combined.d) (spec4.set result.d)


{-| Helper for [`map5`](#map5)
-}
type alias Acc5 a b c d e =
    { a : Ecs.Component a
    , b : Ecs.Component b
    , c : Ecs.Component c
    , d : Ecs.Component d
    , e : Ecs.Component e
    }


{-| Same as [`map2`](#map2) only with 5 components
-}
map5 :
    (( a, a -> System (Acc5 a b c d e) )
     -> ( b, b -> System (Acc5 a b c d e) )
     -> ( c, c -> System (Acc5 a b c d e) )
     -> ( d, d -> System (Acc5 a b c d e) )
     -> ( e, e -> System (Acc5 a b c d e) )
     -> System (Acc5 a b c d e)
    )
    -> Ecs.Component.Spec a world
    -> Ecs.Component.Spec b world
    -> Ecs.Component.Spec c world
    -> Ecs.Component.Spec d world
    -> Ecs.Component.Spec e world
    -> System world
map5 f spec1 spec2 spec3 spec4 spec5 world =
    let
        set1 : Entity -> a -> System (Acc5 a b c d e)
        set1 (Entity i) a acc =
            let
                (Component comp) =
                    acc.a
            in
            { acc | a = Component (Dict.insert i a comp) }

        set2 : Entity -> b -> System (Acc5 a b c d e)
        set2 (Entity i) b acc =
            let
                (Component comp) =
                    acc.b
            in
            { acc | b = Component (Dict.insert i b comp) }

        set3 : Entity -> c -> System (Acc5 a b c d e)
        set3 (Entity i) c acc =
            let
                (Component comp) =
                    acc.c
            in
            { acc | c = Component (Dict.insert i c comp) }

        set4 : Entity -> d -> System (Acc5 a b c d e)
        set4 (Entity i) d acc =
            let
                (Component comp) =
                    acc.d
            in
            { acc | d = Component (Dict.insert i d comp) }

        set5 : Entity -> e -> System (Acc5 a b c d e)
        set5 (Entity i) e acc =
            let
                (Component comp) =
                    acc.e
            in
            { acc | e = Component (Dict.insert i e comp) }

        combined : { a : Component a, b : Component b, c : Component c, d : Component d, e : Component e }
        combined =
            { a = spec1.get world
            , b = spec2.get world
            , c = spec3.get world
            , d = spec4.get world
            , e = spec5.get world
            }

        result : Acc5 a b c d e
        result =
            indexedFoldl
                (\n a acc ->
                    Maybe.map4
                        (\b c d e -> f ( a, set1 n ) ( b, set2 n ) ( c, set3 n ) ( d, set4 n ) ( e, set5 n ) acc)
                        (Ecs.Component.get n acc.b)
                        (Ecs.Component.get n acc.c)
                        (Ecs.Component.get n acc.d)
                        (Ecs.Component.get n acc.e)
                        |> Maybe.withDefault acc
                )
                combined.a
                combined
    in
    world
        |> applyIf (result.a /= combined.a) (spec1.set result.a)
        |> applyIf (result.b /= combined.b) (spec2.set result.b)
        |> applyIf (result.c /= combined.c) (spec3.set result.c)
        |> applyIf (result.d /= combined.d) (spec4.set result.d)


{-| A helper function to pipe into systems

    update msg world =
        world
            |> system1
            |> applyIf (msg === KeyUp "a") systemMoveLeft
            |> system2

-}
applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f world =
    if bool then
        f world

    else
        world


{-| Same as [`applyIf`](#applyIf), but works with `Maybe`

    update msg world =
        world
            |> system1
            |> applyMaybe (decode saveDecoder msg) loadGame
            |> system2

-}
applyMaybe : Maybe a -> (a -> c -> c) -> c -> c
applyMaybe m f world =
    case m of
        Just a ->
            f a world

        Nothing ->
            world
