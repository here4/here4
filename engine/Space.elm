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
import Body exposing (Camera)
import Ground exposing (Ground)

programWithFlags
  : { init : ( model, Cmd (WorldMsg msg) )
    , update : WorldMsg msg -> model -> ( model, Cmd (WorldMsg msg) )
    , label : Maybe Bag.Key -> model -> String
    , view : model -> Maybe Model.World
    , animate : Ground -> Time -> model -> model
    , keyLimit : model -> Int
    , ground : model -> Maybe Ground
    , camera : Maybe Bag.Key -> model -> Maybe Camera
    , focus : Bag.Key -> model -> Maybe Focus
    }
  -> Program Model.Args (Model.Model model) (Model.Msg (WorldMsg msg))
programWithFlags world =
    Html.programWithFlags
        { init = Model.init world.init
        , update = Update.update world.update world.label world.keyLimit world.ground world.animate world.camera world.focus
        , subscriptions = subscriptions
        , view = View.view world.view
        }

{- Subscribe to keychange events.
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
            188 -> \k -> { k | kComma = on }
            190 -> \k -> { k | kPeriod = on }
            73 -> \k -> { k | kI = on }
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
