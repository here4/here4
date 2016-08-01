module TextureCube exposing (create)

import Math.Vector3 exposing (vec3)
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (Texture, loadTexture)

import Thing exposing (..)
import Things.Cube exposing (textureCube)

type alias TextureCube = List Thing

type Msg
    = TextureError WebGL.Error
    | TextureLoaded Texture

create : String -> (Things, Cmd ThingMsg)
create path = createThings (init path)
    { update = update
    , animate = animate
    , things = things
    }

init : String -> (TextureCube, Cmd Msg)
init path = ([], loadTexture path |> Task.perform TextureError TextureLoaded)

update : Msg -> TextureCube -> (TextureCube, Cmd Msg)
update msg model = case msg of
    TextureError err ->
        -- ( { model | message = "Error loading texture" }, Cmd.none )
        ( model, Cmd.none )
    TextureLoaded texture ->
        ( [ put (vec3 -2 20 -17) (textureCube texture) ] , Cmd.none )

animate : Time -> TextureCube -> TextureCube
animate dt cube = cube

things : TextureCube -> List Thing
things = identity
