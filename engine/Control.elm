module Control exposing (..)

import Math.Vector3 exposing (Vec3)

import Bag exposing (Bag)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)

import Ground exposing (Ground)

type alias CtrlMsg a  = Dispatch EffectMsg Msg a
type alias WorldMsg a = DispatchHub Bag.Key EffectMsg Msg Dynamic a

type Msg
    = Move Vec3

type EffectMsg
    = UpdateGround Ground


