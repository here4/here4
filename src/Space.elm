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

import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Model
import Update
import View

import Thing exposing (Focus, Things)
import Things.Terrain exposing (Terrain)

programWithFlags
  : { init : ( model, Cmd (CtrlMsg msg) )
    , update : CtrlMsg msg -> model -> ( model, Cmd (CtrlMsg msg) )
    , view : model -> Maybe Model.World
    , animate : Time -> model -> model 
    , terrain : model -> Maybe Terrain
    , focus : model -> Maybe Focus
    }
  -> Program Model.Args (Model.Model model) (Model.Msg (CtrlMsg msg))
programWithFlags world =
    Html.programWithFlags
        { init = Model.init world.init
        , update = Update.update world.update world.focus world.terrain world.animate
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
