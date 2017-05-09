module TextureCube exposing (create)

import Math.Vector3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)
import Tuple exposing (first)
import WebGL.Texture as Texture exposing (Texture, Error)

import Math.Quaternion as Qn
import Orientation exposing (rotateLabV)

import App exposing (App, AppMsg, Focus, appToFocus)
import Body exposing (..)
import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Model exposing (Inputs)

import Body.Cube exposing (textureCube)
import Vehicles.DreamBuggy exposing (dreamBuggy)

type alias Model = Maybe (Body, Vec3)

type Msg
    = TextureLoaded (Result Error Texture)

create : String -> String -> (App, Cmd AppMsg)
create label path = App.create (init path)
    { label = always label
    , update = update
    , animate = animate
    , bodies = bodies
    , camera = camera
    , focus = focus
    }

init : String -> (Model, Cmd (CtrlMsg Msg))
init path =
    ( Nothing
    , Texture.load path
        |> Task.attempt (Self << TextureLoaded)
    )

update : CtrlMsg Msg -> Model
    -> (Model, Cmd (CtrlMsg Msg))
update msg model = case msg of
    Self (TextureLoaded textureResult) ->
        case textureResult of
            Ok texture ->
                ( Just (put (vec3 -2 20 -17) (textureCube texture), vec3 0 0 0) , Cmd.none )
            Err msg ->
                -- ( { model | message = "Error loading texture" }, Cmd.none )
                ( model, Cmd.none )

    Ctrl (Control.Move dp) -> case model of
        Just (body, vel) -> (Just (translate dp body, vel), Cmd.none)
        Nothing          -> (Nothing, Cmd.none)

    Ctrl (Control.Drive inputs) ->
        (drive inputs model, Cmd.none)

    Effect _ ->
        (model, Cmd.none)

animate : Time -> Model -> Model
animate dt model = model

bodies : Model -> List Body
bodies model = case model of
    Just (body, _) -> [body]
    Nothing        -> []

camera : Model -> Maybe Camera
camera model = Maybe.map (bodyCamera << first) model

focus : Model -> Maybe Focus
focus model = Maybe.map (appToFocus << first) model
    
drive : Inputs -> Model -> Model
drive inputs model = case model of
    Just (BCtr anchor scale p o appear, vel) ->
        let motion0 = { position = p, orientation = Qn.fromVec3 o, velocity = vel }
            motion = dreamBuggy.move Nothing (always 1.0) inputs motion0
        in
            Just ( BCtr anchor scale motion.position (rotateLabV motion.orientation (vec3 0 0 1)) appear
                 , motion.velocity)
    Nothing -> Nothing
