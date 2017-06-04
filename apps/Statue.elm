module Statue exposing (create)

import Math.Vector3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)

import App exposing (App, AppMsg, Focus, appToFocus)
import Appearance exposing (Appearance)
import Body exposing (..)
import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Ground exposing (Ground)

import Orientation
import Vehicles.DreamBuggy as DreamBuggy

type alias Model = Moving Body

type alias Msg = ()

create : String -> Vec3 -> Appearance -> (App, Cmd AppMsg)
create label pos appear = App.create (init pos appear)
    { label = always label
    , update = update
    , animate = animate
    , bodies = bodies
    , camera = camera
    , focus = focus
    }

init : Vec3 -> Appearance -> (Model, Cmd (CtrlMsg Msg))
init pos appear =
    ( { anchor = AnchorGround
      , scale = vec3 1 1 1
      , position = pos
      , orientation = Orientation.initial
      , appear = appear
      , velocity = vec3 0 0 0
      }
    , Cmd.none )

update : CtrlMsg Msg -> Model -> (Model, Cmd (CtrlMsg Msg))
update msg model = case msg of
    Ctrl (Control.Move dp) ->
        ( translate dp model, Cmd.none )

    Ctrl (Control.Drive ground inputs) ->
        ( DreamBuggy.drive ground inputs model, Cmd.none )

    _ ->
        ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt body = body

bodies : Model -> List Body
bodies body = [toBody body]

camera : Model -> Maybe Camera
camera model = Just (bodyCamera model)

focus : Model -> Maybe Focus
focus body = Just (appToFocus body)
    
