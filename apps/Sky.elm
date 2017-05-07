module Sky exposing (create)

import Math.Vector3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)

import App exposing (App, AppMsg, Focus, appToFocus)
import Appearance exposing (Appearance)
import Body exposing (Body, anchorSky, put, resize, translate)
import Control exposing (CtrlMsg)
import Dispatch exposing (..)

type alias Model = Body

type alias Msg = ()

create : Appearance -> (App, Cmd AppMsg)
create appear = App.create (init appear)
    { update = update
    , animate = animate
    , bodies = bodies
    , focus = focus
    }

init : Appearance -> (Model, Cmd (CtrlMsg Msg))
init appear = ( anchorSky <| resize 80 <| put (vec3 0 1 1) appear, Cmd.none )

update : CtrlMsg Msg -> Model -> (Model, Cmd (CtrlMsg Msg))
update msg model = case msg of
    Self _                 -> ( model, Cmd.none)
    Down (Control.Move dp) -> ( translate dp model, Cmd.none )

animate : Time -> Model -> Model
animate dt body = body

bodies : Model -> List Body
bodies body = [body]

focus : Model -> Maybe Focus
focus body = Just (appToFocus body)
    
