module Terrain exposing (create)

import Math.Vector3 exposing (vec3)
import Task exposing (Task)
import Time exposing (Time)
import WebGL.Texture as Texture exposing (Texture, Error)

import App exposing (App, AppMsg, Focus, appToFocus)
import Body exposing (Body, translate, put)
import Control exposing (..)
import Dispatch exposing (..)
import Ground exposing (Ground)

type alias Model = List Body

type Msg
    = TerrainGenerated (Ground, List Body)

create : (((Ground, List Body) -> Msg) -> Cmd Msg)
    -> (App, Cmd AppMsg)
create makeGround = App.create (init makeGround)
    { update = update
    , animate = animate
    , bodies = bodies
    , focus = focus
    }

init : (((Ground, List Body) -> Msg) -> Cmd Msg)
    -> (Model, Cmd (CtrlMsg Msg))
init makeGround =
    ( []
    , Cmd.map Self (makeGround TerrainGenerated)
    )

update : CtrlMsg Msg -> Model
    -> (Model, Cmd (CtrlMsg Msg))
update msg model = case msg of
    Self (TerrainGenerated (ground, bodies)) ->
            ( bodies
            , Task.succeed ground
                |> Task.perform (Effect << UpdateGround)
            )
    _ -> (model, Cmd.none)

animate : Time -> Model -> Model
animate dt model = model

bodies : Model -> List Body
bodies = identity

focus : Model -> Maybe Focus
focus model = Nothing
    
