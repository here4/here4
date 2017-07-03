module Control exposing (..)

import Math.Vector3 exposing (Vec3)
import Bag exposing (Bag)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Ground exposing (Ground)
import Model exposing (Inputs)


type alias CtrlMsg a =
    Dispatch EffectMsg Msg a

type WorldKey
    = ToApp Bag.Key
    | ToParty Bag.Key

type alias WorldMsg a =
    DispatchHub WorldKey EffectMsg Msg Dynamic a


type Msg
    = Move Vec3
    | Drive Ground Inputs


type EffectMsg
    = UpdateGround Ground
