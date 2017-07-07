module MultiWorld exposing (create, Model, Msg)

import Control exposing (..)
import Dispatch exposing (..)
import Model exposing (Args)
import World exposing (..)
import App exposing (..)
import Ground exposing (Ground)


type alias MultiWorldMsg =
    ()


type alias Msg =
    WorldMsg MultiWorldMsg


type alias MultiWorldModel =
    ()


type alias Model =
    WorldModel MultiWorldModel


create :
    List { apps : List ( App, Cmd AppMsg ), defaultSelf : ( App, Cmd AppMsg) }
    -> Program Args (Model.Model Model Msg) (Model.Msg Msg)
create details =
    World.create init update details


init : ( MultiWorldModel, Cmd MultiWorldMsg )
init =
    ( (), Cmd.none )


update : MultiWorldMsg -> MultiWorldModel -> ( MultiWorldModel, Cmd MultiWorldMsg )
update msg model =
    ( model, Cmd.none )
