module Methods exposing (..)

import Html exposing (Html)
import App exposing (Focus)
import Camera exposing (Framing, Shot)
import Control exposing (..)
import Ground exposing (Ground)
import Model exposing (..)
import Time exposing (Time)

type alias Methods worldModel worldMsg =
    { init : ( worldModel, Cmd (WorldMsg worldMsg) )
    , update : WorldMsg worldMsg -> worldModel -> ( worldModel, Cmd (WorldMsg worldMsg) )
    , label : WorldKey PartyKey -> worldModel -> String
    , overlay : WorldKey PartyKey -> worldModel -> Html (WorldMsg worldMsg)
    , view : WorldKey () -> worldModel -> Maybe Model.World
    , animate : WorldKey () -> Ground -> Time -> worldModel -> worldModel
    , join : WorldKey () -> worldModel -> (Maybe (WorldKey PartyKey), worldModel, Cmd (WorldMsg worldMsg))
    , leave : WorldKey PartyKey -> worldModel -> worldModel
    , changeRide : WorldKey PartyKey -> worldModel -> ( worldModel, Cmd (WorldMsg worldMsg) )
    , ground : WorldKey () -> worldModel -> Maybe Ground
    , framing : WorldKey PartyKey -> worldModel -> Maybe Framing
    , focus : WorldKey AppKey -> worldModel -> Maybe Focus
    }

