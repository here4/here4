module Control exposing (..)

import App.Control
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Model exposing (GlobalMsg, Inputs, WorldKey(..), AppKey, PartyKey)


type Route
    = ToApp (WorldKey AppKey)
    | ToParty (WorldKey PartyKey)


type alias WorldMsg a =
    DispatchHub Route (App.Control.EffectMsg (WorldKey ())) App.Control.Msg Dynamic GlobalMsg a
