module Control exposing (..)

import Math.Vector3 exposing (Vec3)
import Bag exposing (Bag)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Ground exposing (Ground)
import Model exposing (Inputs, Location, AppKey, PartyKey)


type alias CtrlMsg a =
    Dispatch EffectMsg Msg a

type WorldKey
    = ToApp AppKey
    | ToParty PartyKey

type alias WorldMsg a =
    DispatchHub WorldKey EffectMsg Msg Dynamic a


type Msg
    = Move Vec3
    | Enter Bag.Key -- Player @key enters the receiving app
    | Leave Bag.Key -- Player @key leaves the receiving app
    | Drive Ground Inputs


type EffectMsg
    = UpdateGround Ground
    | RelocateParty Bag.Key Location
