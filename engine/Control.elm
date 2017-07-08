module Control exposing (..)

import Math.Vector3 exposing (Vec3)
import Bag exposing (Bag)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Ground exposing (Ground)
import Location exposing (..)
import Model exposing (GlobalMsg, Inputs, WorldKey(..), AppKey, PartyKey)


type alias CtrlMsg a =
    Dispatch (EffectMsg ()) Msg a

type Route
    = ToApp (WorldKey AppKey)
    | ToParty (WorldKey PartyKey)

type alias WorldMsg a =
    DispatchHub Route (EffectMsg (WorldKey ())) Msg Dynamic GlobalMsg a

type Msg
    = Move Vec3
    | Enter PartyKey -- Player @key enters the receiving app
    | Leave PartyKey -- Player @key leaves the receiving app
    | Drive Ground Inputs


type EffectMsg worldKey
    = UpdateGround worldKey Ground
    | RelocateParty worldKey PartyKey Location

