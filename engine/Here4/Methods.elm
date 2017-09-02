module Here4.Methods exposing (..)

import Here4.App.Types exposing (Focus)
import Here4.Camera.Types exposing (Framing, Shot)
import Here4.Control exposing (..)
import Here4.Ground exposing (Ground)
import Here4.Model as Model exposing (..)
import Html exposing (Html)
import Time exposing (Time)


type alias Methods worldFlags worldModel worldMsg =
    { init : worldFlags -> ( worldModel, Cmd (WorldMsg worldMsg) )
    , update : WorldMsg worldMsg -> worldModel -> ( worldModel, Cmd (WorldMsg worldMsg) )
    , worldId : WorldKey () -> worldModel -> Maybe String
    , worldLabel : WorldKey () -> worldModel -> Maybe String
    , partyLabel : WorldKey PartyKey -> worldModel -> String
    , overlay : WorldKey PartyKey -> worldModel -> Html (WorldMsg worldMsg)
    , view : WorldKey () -> worldModel -> Maybe Model.World
    , animate : WorldKey () -> Ground -> Time -> worldModel -> worldModel
    , join : WorldKey () -> worldModel -> ( Maybe (WorldKey PartyKey), worldModel, Cmd (WorldMsg worldMsg) )
    , leave : WorldKey PartyKey -> worldModel -> worldModel
    , changeRide : WorldKey PartyKey -> worldModel -> ( worldModel, Cmd (WorldMsg worldMsg) )
    , ground : WorldKey () -> worldModel -> Maybe Ground
    , framing : WorldKey PartyKey -> worldModel -> Maybe Framing
    , focus : WorldKey AppKey -> worldModel -> Maybe Focus
    }
