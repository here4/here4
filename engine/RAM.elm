module RAM exposing (create, Model, Msg)

import Control exposing (..)
import Dispatch exposing (..)
import Model exposing (Args)
import World exposing (..)
import App exposing (..)


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
