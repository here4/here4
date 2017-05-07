module Control exposing (..)

import Math.Vector3 exposing (Vec3)

import Bag exposing (Bag)
import Dispatch exposing (Dispatch)
import Dynamic exposing (Dynamic)

type alias CtrlMsg a = Dispatch Msg a

type Msg
    = Move Vec3

type WorldMsg a
    = W a
    | Send Bag.Key (CtrlMsg Dynamic)
    | Ctrl Msg

