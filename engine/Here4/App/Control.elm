module Here4.App.Control exposing (..)

import Here4.Dispatch exposing (..)
import Here4.Ground exposing (Ground)
import Here4.Location exposing (Location)
import Here4.Model exposing (Inputs, PartyKey)
import Math.Vector3 exposing (Vec3)


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
