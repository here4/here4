module Control exposing (..)

import Math.Vector3 exposing (Vec3)
import Bag exposing (Bag)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Ground exposing (Ground)
import Model exposing (Inputs, Location, WorldKey(..), AppKey, PartyKey)


type alias CtrlMsg a =
    Dispatch EffectMsg Msg a

type Route
    = ToApp (WorldKey AppKey)
    | ToParty (WorldKey PartyKey)

type alias WorldMsg a =
    DispatchHub Route EffectMsg Msg Dynamic a


type Msg
    = Move Vec3
    | Enter Bag.Key -- Player @key enters the receiving app
    | Leave Bag.Key -- Player @key leaves the receiving app
    | Drive Ground Inputs


type EffectMsg
    = UpdateGround Ground
    | RelocateParty Bag.Key Location
