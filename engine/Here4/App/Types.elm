module Here4.App.Types exposing
    ( Animated
    , App
    , AppModel
    , AppMsg
    , AppPosition
    , CtrlMsg
    , Msg (..)
    , EffectMsg (..)
    , Focus
    )

import Dynamic exposing (Dynamic)
import Here4.Body exposing (..)
import Here4.Camera.Types exposing (Framing)
import Here4.Dispatch exposing (..)
import Here4.Ground exposing (Ground)
import Here4.Location exposing (Location)
import Here4.Model exposing (Inputs, PartyKey)
import Here4.Orientation exposing (Orientation)
import Html exposing (Html)
import Math.Vector3 exposing (Vec3)
import Time exposing (Time)


type alias Animated model msg =
    { id : model -> String
    , label : model -> String
    , update : msg -> model -> ( model, Cmd msg )
    , bodies : model -> (Vec3 -> List Body)
    , animate : Ground -> Time -> model -> model
    , framing : PartyKey -> model -> Maybe Framing
    , focus : model -> Maybe Focus
    , overlay : model -> Html msg
    , reposition : Maybe AppPosition -> model -> model
    }


type alias AppPosition =
    { position : Vec3
    , orientation : Orientation
    }


type alias AppModel =
    Dynamic


type alias AppMsg =
    CtrlMsg Dynamic


type alias App =
    { methods : Animated AppModel AppMsg
    , model : AppModel
    }

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
    | AddApp worldKey (App, Cmd AppMsg)


type alias Focus =
    { position : Vec3
    }

