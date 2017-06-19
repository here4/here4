module NewWorld exposing (create, Model, Msg)

import Control exposing (..)
import Dispatch exposing (..)
import Model exposing (Args)
import World exposing (..)
import App exposing (..)
import Ground exposing (Ground)


type alias NewWorldMsg =
    ()


type alias Msg =
    WorldMsg NewWorldMsg


type alias NewWorldModel =
    ()


type alias Model =
    WorldModel NewWorldModel


create :
    { apps : List ( App, Cmd AppMsg ) }
    -> Program Args (Model.Model Model Msg) (Model.Msg Msg)
create details =
    World.create init update details


init : ( NewWorldModel, Cmd NewWorldMsg )
init =
    ( (), Cmd.none )


update : NewWorldMsg -> NewWorldModel -> ( NewWorldModel, Cmd NewWorldMsg )
update msg model =
    ( model, Cmd.none )
