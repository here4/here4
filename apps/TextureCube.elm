module TextureCube exposing (create)

import Math.Vector3 exposing (vec3)
import Task exposing (Task)
import Time exposing (Time)
import WebGL.Texture as Texture exposing (Texture, Error)

import App exposing (App, AppMsg, Focus, appToFocus)
import Body exposing (Body, translate, put)
import Control exposing (CtrlMsg)
import Dispatch exposing (..)

import Body.Cube exposing (textureCube)

type alias TextureCube = List Body

type Msg
    = TextureLoaded (Result Error Texture)

create : String -> (App, Cmd AppMsg)
create path = App.create (init path)
    { update = update
    , animate = animate
    , bodies = bodies
    , focus = focus
    }

init : String -> (TextureCube, Cmd (CtrlMsg Msg))
init path =
    ( []
    , Texture.load path
        |> Task.attempt (Self << TextureLoaded)
    )

update : CtrlMsg Msg -> TextureCube
    -> (TextureCube, Cmd (CtrlMsg Msg))
update msg model = case msg of
    Self (TextureLoaded textureResult) ->
        case textureResult of
          Ok texture ->
            ( [ put (vec3 -2 20 -17) (textureCube texture) ] , Cmd.none )
          Err msg ->
            -- ( { model | message = "Error loading texture" }, Cmd.none )
            ( model, Cmd.none )
    Ctrl (Control.Move dp) -> ( List.map (translate dp) model, Cmd.none )

animate : Time -> TextureCube -> TextureCube
animate dt cube = cube

bodies : TextureCube -> List Body
bodies = identity

focus : TextureCube -> Maybe Focus
focus cube = Maybe.map appToFocus (List.head cube)
    
