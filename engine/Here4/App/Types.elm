module Here4.App.Types exposing (Animated, App, AppModel, AppMsg, AppPosition, Focus)

import Dynamic exposing (Dynamic)
import Here4.App.Control exposing (..)
import Here4.Body exposing (..)
import Here4.Camera.Types exposing (Framing)
import Here4.Ground exposing (Ground)
import Here4.Model exposing (PartyKey)
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

type alias Focus =
    { position : Vec3
    }
