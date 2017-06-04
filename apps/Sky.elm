module Sky exposing (create)

import Math.Vector3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)

import App exposing (App, AppMsg, Focus, appToFocus)
import Appearance exposing (Appearance)
import Body exposing (..)
import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Ground exposing (Ground)

type alias Model = Body

type alias Msg = ()

create : Appearance -> (App, Cmd AppMsg)
create appear = App.create (init appear)
    { label = always "Sky"
    , update = update
    , animate = animate
    , bodies = bodies
    , camera = always Nothing
    , focus = always Nothing
    }

init : Appearance -> (Model, Cmd (CtrlMsg Msg))
init appear = ( anchorSky <| resize 80 <| put (vec3 0 1 1) appear, Cmd.none )

update : CtrlMsg Msg -> Model -> (Model, Cmd (CtrlMsg Msg))
update msg model = case msg of
    Ctrl (Control.Move dp) -> ( translate dp model, Cmd.none )
    _                      -> ( model, Cmd.none)

animate : Ground -> Time -> Model -> Model
animate ground dt body = body

bodies : Model -> List Body
bodies body = [body]
