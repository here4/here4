module Space exposing (programWithFlags)

{-| This module drives a virtuul wurld


# Program entry

@docs main

-}

import AnimationFrame
import Html exposing (Html)
import Keyboard.Extra
import KeyboardInput
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
import Camera exposing (Framing, Shot)
import Ground exposing (Ground)

programWithFlags :
    { init : ( model, Cmd (WorldMsg msg) )
    , update : WorldMsg msg -> model -> ( model, Cmd (WorldMsg msg) )
    , label : Maybe Bag.Key -> model -> String
    , overlay : Maybe Bag.Key -> model -> Html (WorldMsg msg)
    , view : model -> Maybe Model.World
    , animate : Ground -> Time -> model -> model
    , join : model -> (model, Bag.Key)
    , leave : Bag.Key -> model -> model
    , keyLimit : model -> Int
    , ground : model -> Maybe Ground
    , framing : Maybe Bag.Key -> model -> Maybe Framing
    , focus : Bag.Key -> model -> Maybe Focus
    }
    -> Program Model.Args (Model.Model model (WorldMsg msg)) (Model.Msg (WorldMsg msg))
programWithFlags world =
    Html.programWithFlags
        { init = Model.init world.init
        , update = Update.update world.update world.label world.overlay world.keyLimit world.ground world.animate world.join world.leave world.framing world.focus
        , subscriptions = subscriptions
        , view = View.view world.view
        }



subscriptions : Model.Model worldModel worldMsg -> Sub (Model.Msg worldMsg)
subscriptions model =
    [ AnimationFrame.diffs (Model.Animate << Time.inSeconds)
    , Keyboard.Extra.downs (KeyboardInput.keyChange True)
    , Keyboard.Extra.ups (KeyboardInput.keyChange False)
    , Window.resizes Model.Resize
    , Ports.isLocked Model.LockUpdate
    ]
        ++ (if model.isLocked then
                [ Ports.movement Model.MouseMove ]
            else
                [ Mouse.clicks (\_ -> Model.LockRequest True) ]
           )
        |> Sub.batch
