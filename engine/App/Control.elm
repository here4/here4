module App.Control exposing (..)

import Dispatch exposing (..)
import Ground exposing (Ground)
import Location exposing (Location)
import Math.Vector3 exposing (Vec3)
import Model exposing (Inputs, PartyKey)


type alias CtrlMsg a =
    Dispatch (EffectMsg ()) Msg a


type Msg
    = Move Vec3
    | Enter PartyKey -- Player @key enters the receiving app
    | Leave PartyKey -- Player @key leaves the receiving app
    | Drive Ground Inputs


type EffectMsg worldKey
    = UpdateGround worldKey Ground
    | RelocateParty worldKey PartyKey Location
