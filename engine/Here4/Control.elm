module Here4.Control exposing (..)

import Dynamic exposing (Dynamic)
import Here4.App.Types as App
import Here4.Dispatch exposing (..)
import Here4.Model exposing (GlobalMsg, NavigatorMsg, Inputs, WorldKey(..), AppKey, PartyKey)


type Route
    = ToApp (WorldKey AppKey)
    | ToParty (WorldKey PartyKey)


type alias WorldMsg a =
    DispatchHub Route (App.EffectMsg (WorldKey ()) (Maybe AppKey)) App.Msg Dynamic GlobalMsg NavigatorMsg a
