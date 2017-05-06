module Control exposing (..)

import Math.Vector3 exposing (Vec3)

import Dispatch exposing (Dispatch)

type Msg
    = Move Vec3

type alias CtrlMsg a = Dispatch Msg a
