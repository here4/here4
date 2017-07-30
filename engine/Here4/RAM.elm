module Here4.RAM exposing (create, Model, Msg)

import Here4.App exposing (..)
import Here4.Control exposing (..)
import Here4.Dispatch exposing (..)
import Here4.Model exposing (Args)
import Here4.World exposing (..)


type alias RAMMsg =
    ()


type alias Msg =
    WorldMsg RAMMsg


type alias RAMModel =
    ()


type alias Model =
    Multiverse RAMModel


create :
    List World.Attributes
    -> Program Args (Model.Model Model Msg) (Model.Msg Msg)
create attributes =
    World.create init update attributes


init : ( RAMModel, Cmd RAMMsg )
init =
    ( (), Cmd.none )


update : RAMMsg -> RAMModel -> ( RAMModel, Cmd RAMMsg )
update msg model =
    ( model, Cmd.none )
