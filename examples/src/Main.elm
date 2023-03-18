module Main exposing (main)

import Browser
import Browser.Events
import Ecs
import Ecs.Component
import Ecs.Config
import Ecs.Entity
import Ecs.System exposing (System, applyIf)
import Html exposing (..)
import Html.Attributes exposing (style)


main : Program () World Float
main =
    Browser.element
        { init = \_ -> ( spawn world, Cmd.none )
        , update = \_ w -> ( system velocitySpec positionSpec w, Cmd.none )
        , subscriptions = \_ -> Browser.Events.onAnimationFrameDelta identity
        , view =
            \w ->
                Ecs.System.foldl
                    (\( px, py ) acc ->
                        div
                            [ style "width" "30px"
                            , style "height" "30px"
                            , style "position" "absolute"
                            , style "top" "0"
                            , style "left" "0"
                            , style "background" "red"
                            , style "transform" ("translate(" ++ String.fromInt px ++ "px, " ++ String.fromInt py ++ "px)")
                            ]
                            []
                            :: acc
                    )
                    (positionSpec.get w)
                    []
                    |> div []
        }


system : Ecs.Component.Spec ( Int, Int ) World -> Ecs.Component.Spec ( Int, Int ) World -> System World
system spec1 spec2 w =
    Ecs.System.map2
        (\( ( vx, vy ), setVel ) ( ( px, py ), setPos ) acc ->
            let
                x =
                    vx + px

                y =
                    vy + py
            in
            acc
                |> setPos ( x, y )
                |> applyIf (x < 0) (setVel ( abs vx, vy ))
                |> applyIf (x > w.windowWidth) (setVel ( abs vx * -1, vy ))
                |> applyIf (y < 0) (setVel ( vx, abs vy ))
                |> applyIf (y > w.windowHeight) (setVel ( vx, abs vy * -1 ))
        )
        spec1
        spec2
        w


type alias World =
    { ecsConfig : Ecs.Config
    , position : Ecs.Component ( Int, Int )
    , velocity : Ecs.Component ( Int, Int )
    , windowWidth : Int
    , windowHeight : Int
    }


world : World
world =
    { ecsConfig = Ecs.Config.init
    , position = Ecs.Component.empty
    , velocity = Ecs.Component.empty
    , windowWidth = 800
    , windowHeight = 600
    }


ecsConfigSpec : Ecs.Config.Spec World
ecsConfigSpec =
    { get = .ecsConfig
    , set = \ecsConfig w -> { w | ecsConfig = ecsConfig }
    }


spawn : World -> World
spawn w_ =
    List.range 0 10
        |> List.foldl
            (\i ->
                Ecs.Entity.create ecsConfigSpec
                    >> Ecs.Entity.with ( positionSpec, ( i * 3, i * 5 ) )
                    >> Ecs.Entity.with ( velocitySpec, ( modBy 3 i + 1, modBy 5 i + 1 ) )
                    >> Tuple.second
            )
            w_


positionSpec : Ecs.Component.Spec ( Int, Int ) { world | position : Ecs.Component ( Int, Int ) }
positionSpec =
    Ecs.Component.Spec .position (\comps w -> { w | position = comps })


velocitySpec : Ecs.Component.Spec ( Int, Int ) { world | velocity : Ecs.Component ( Int, Int ) }
velocitySpec =
    Ecs.Component.Spec .velocity (\comps w -> { w | velocity = comps })
