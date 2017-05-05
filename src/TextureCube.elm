module TextureCube exposing (create)

import Math.Vector3 exposing (vec3)
import Task exposing (Task)
import Time exposing (Time)
import WebGL.Texture as Texture exposing (Texture, Error)

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
    , things = things
    , focus = focus
    }

init : String -> (TextureCube, Cmd (Dispatch CtrlMsg Msg))
init path =
    ( []
    , Texture.load path
        |> Task.attempt (Self << TextureLoaded)
    )

update : Dispatch CtrlMsg Msg -> TextureCube -> (TextureCube, Cmd (Dispatch CtrlMsg Msg))
update msg model = case msg of
    Self (TextureLoaded textureResult) ->
        case textureResult of
          Ok texture ->
            ( [ put (vec3 -2 20 -17) (textureCube texture) ] , Cmd.none )
          Err msg ->
            -- ( { model | message = "Error loading texture" }, Cmd.none )
            ( model, Cmd.none )
    Down (Move dp) -> ( List.map (translate dp) model, Cmd.none )

animate : Time -> TextureCube -> TextureCube
animate dt cube = cube

things : TextureCube -> List Body
things = identity

focus : TextureCube -> Maybe Focus
focus cube = Maybe.map thingToFocus (List.head cube)
    
