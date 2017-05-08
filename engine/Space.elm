module Space exposing (programWithFlags)

{-| This module drives a virtuul wurld

# Program entry
@docs main
-}

import AnimationFrame
import Html
import Keyboard
import Mouse
import Time exposing (Time)
import Window

import Ports

import Bag
import Control exposing (WorldMsg)
import Dispatch exposing (..)
import Model
import Update
import View

import App exposing (Focus)
import Ground exposing (Ground)

programWithFlags
  : { init : ( model, Cmd (WorldMsg msg) )
    , update : WorldMsg msg -> model -> ( model, Cmd (WorldMsg msg) )
    , view : model -> Maybe Model.World
    , animate : Time -> model -> model 
    , ground : model -> Maybe Ground
    , focus : model -> Maybe (Bag.Key, Focus)
    }
  -> Program Model.Args (Model.Model model) (Model.Msg (WorldMsg msg))
programWithFlags world =
    Html.programWithFlags
        { init = Model.init world.init
        , update = Update.update world.update world.focus world.ground world.animate
        , subscriptions = subscriptions
        , view = View.view world.view
        }

{- Subscribe to keychange events.
Ignore anything that isn't an escape, space or WASD keys.
-}
keyChange : Bool -> Keyboard.KeyCode -> Model.Msg worldMsg
keyChange on keyCode =
    if keyCode == 27 && on then
        Model.LockRequest False
    else
        (case keyCode of
            32 -> \k -> { k | space = on }
            65 -> \k -> { k | left  = on }
            68 -> \k -> { k | right = on }
            87 -> \k -> { k | up    = on }
            83 -> \k -> { k | down  = on }
            _  -> Basics.identity
        ) |> Model.KeyChange

subscriptions : Model.Model worldModel -> Sub (Model.Msg worldMsg)
subscriptions model =
    [ AnimationFrame.diffs (Model.Animate << Time.inSeconds)
    , Keyboard.downs (keyChange True)
    , Keyboard.ups (keyChange False)
    , Window.resizes Model.Resize
    , Ports.isLocked Model.LockUpdate
    ]
        ++ (if model.isLocked then
                [ Ports.movement Model.MouseMove ]
            else
                [ Mouse.clicks (\_ -> Model.LockRequest True) ]
           )
        |> Sub.batch
