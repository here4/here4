module Control exposing (..)

import Math.Vector3 exposing (Vec3)

import Bag exposing (Bag)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)

type alias CtrlMsg a = Dispatch Msg a

type Msg
    = Move Vec3

type alias WorldMsg a = DispatchHub Bag.Key Msg Dynamic a

