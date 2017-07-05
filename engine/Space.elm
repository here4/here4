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
import Control exposing (WorldMsg)
import Dispatch exposing (..)
import Model exposing (AppKey, PartyKey)
import Update
import View
import App exposing (Focus)
import Camera exposing (Framing, Shot)
import Ground exposing (Ground)

programWithFlags :
    { init : ( model, Cmd (WorldMsg msg) )
    , update : WorldMsg msg -> model -> ( model, Cmd (WorldMsg msg) )
    , label : Maybe PartyKey -> model -> String
    , overlay : Maybe PartyKey -> model -> Html (WorldMsg msg)
    , view : model -> Maybe Model.World
    , animate : Ground -> Time -> model -> model
    , join : model -> (PartyKey, model, Cmd (WorldMsg msg))
    , leave : PartyKey -> model -> model
    , changeRide : PartyKey -> model -> ( model, Cmd (WorldMsg msg) )
    , ground : model -> Maybe Ground
    , framing : Maybe PartyKey -> model -> Maybe Framing
    , focus : AppKey -> model -> Maybe Focus
    }
    -> Program Model.Args (Model.Model model (WorldMsg msg)) (Model.Msg (WorldMsg msg))
programWithFlags world =
    Html.programWithFlags
        { init = Model.init world.init
        , update = Update.update world.update world.label world.overlay world.ground world.animate world.join world.leave world.changeRide world.framing world.focus
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
