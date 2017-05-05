module TextureCube exposing (create)

import Math.Vector3 exposing (vec3)
import Task exposing (Task)
import Time exposing (Time)
import WebGL.Texture as Texture exposing (Texture, Error)

import Body exposing (Body, translate, put)
import Control
import Dispatch exposing (..)
import Thing exposing (..)
import Things.Cube exposing (textureCube)

type alias TextureCube = List Body

type Msg
    = TextureLoaded (Result Error Texture)

create : String -> (Things, Cmd ThingMsg)
create path = createThings (init path)
    { update = update
    , animate = animate
    , bodies = bodies
    , focus = focus
    }

init : String -> (TextureCube, Cmd (Dispatch Control.Msg Msg))
init path =
    ( []
    , Texture.load path
        |> Task.attempt (Self << TextureLoaded)
    )

update : Dispatch Control.Msg Msg -> TextureCube
    -> (TextureCube, Cmd (Dispatch Control.Msg Msg))
update msg model = case msg of
    Self (TextureLoaded textureResult) ->
        case textureResult of
          Ok texture ->
            ( [ put (vec3 -2 20 -17) (textureCube texture) ] , Cmd.none )
          Err msg ->
            -- ( { model | message = "Error loading texture" }, Cmd.none )
            ( model, Cmd.none )
    Down (Control.Move dp) -> ( List.map (translate dp) model, Cmd.none )

animate : Time -> TextureCube -> TextureCube
animate dt cube = cube

bodies : TextureCube -> List Body
bodies = identity

focus : TextureCube -> Maybe Focus
focus cube = Maybe.map thingToFocus (List.head cube)
    
