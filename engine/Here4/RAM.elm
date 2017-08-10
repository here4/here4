module Here4.RAM exposing (create, Flags, Model, Msg)

import Here4.App exposing (..)
import Here4.Control exposing (..)
import Here4.Dispatch exposing (..)
import Here4.Navigator.Control exposing (NavMsg)
import Here4.Model as Model
import Here4.World as World exposing (..)
import Task


type alias Flags =
    ()


type alias RAMMsg =
    ()


type alias Msg =
    WorldMsg (NavMsg RAMMsg)


type alias RAMModel =
    ()


type alias Model =
    Multiverse RAMModel


create :
    List World.Attributes
    -> Program Flags (Model.Model Model Msg) (Model.Msg Msg)
create attributes =
    World.create init update subscriptions attributes


init : Flags -> ( RAMModel, Cmd (NavMsg RAMMsg) )
init flags =
    ( (), Cmd.none )


update : NavMsg RAMMsg -> RAMModel -> ( RAMModel, Cmd (NavMsg RAMMsg) )
update msg model =
    ( model, Cmd.none )


subscriptions : Multiverse RAMModel -> Sub (NavMsg RAMMsg)
subscriptions model =
    Sub.none
