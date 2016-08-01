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

init : String -> (TextureCube, Cmd (MyMsg Msg))
init path =
    ( []
    , loadTexture path
        |> Task.perform (My << TextureError) (My << TextureLoaded)
    )

update : MyMsg Msg -> TextureCube -> (TextureCube, Cmd (MyMsg Msg))
update msg model = case msg of
    My (TextureError err) ->
        -- ( { model | message = "Error loading texture" }, Cmd.none )
        ( model, Cmd.none )
    My (TextureLoaded texture) ->
        ( [ put (vec3 -2 20 -17) (textureCube texture) ] , Cmd.none )
    _ -> ( model, Cmd.none )

animate : Time -> TextureCube -> TextureCube
animate dt cube = cube

things : TextureCube -> List Thing
things = identity
