module Statue exposing (create, VehicleType(..))

import Math.Vector3 as V3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)
import App exposing (App, AppMsg, Focus, appToFocus)
import Appearance exposing (Appearance)
import Body exposing (..)
import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Ground exposing (Ground)
import Model exposing (Motion, motionCamera)
import Orientation
import Vehicles.DreamBuggy as DreamBuggy
import Vehicles.DreamBird as DreamBird
import Vehicles.LookAt as LookAt


type VehicleType
    = Buggy
    | Bird


type alias Model =
    { body : Moving Body
    , speed : Float
    , camera : Motion
    }


type alias Msg =
    ()


create : VehicleType -> Float -> String -> Vec3 -> Appearance -> ( App, Cmd AppMsg )
create vtype speed label pos appear =
    App.create (init pos speed appear)
        { label = always label
        , update = update vtype
        , animate = animate
        , bodies = bodies
        , camera = camera
        , focus = focus
        }


init : Vec3 -> Float -> Appearance -> ( Model, Cmd (CtrlMsg Msg) )
init pos speed appear =
    ( { body =
            { anchor = AnchorGround
            , scale = vec3 1 1 1
            , position = pos
            , orientation = Orientation.initial
            , appear = appear
            , velocity = vec3 0 0 0
            }
      , speed = speed
      , camera =
            { position = V3.add pos (vec3 0 0 -5)
            , velocity = vec3 0 0 0
            , orientation = Orientation.initial
            }
      }
    , Cmd.none
    )


update : VehicleType -> CtrlMsg Msg -> Model -> ( Model, Cmd (CtrlMsg Msg) )
update vtype msg model =
    case msg of
        Ctrl (Control.Move dp) ->
            ( { model | body = translate dp model.body }, Cmd.none )

        Ctrl (Control.Drive ground inputs) ->
            case vtype of
                Buggy ->
                    ( { model | body = DreamBuggy.drive ground model.speed inputs model.body }, Cmd.none )

                Bird ->
                    ( { model | body = DreamBird.drive ground inputs model.body }, Cmd.none )

        _ ->
            ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt model =
    let
        setElevation pos = V3.setY (ground.elevation pos) pos
        onGround body = { body | position = setElevation body.position }
    in
        { model | body = onGround model.body }


bodies : Model -> List Body
bodies model =
    [ toBody model.body ]


camera : Model -> Maybe Camera
camera model =
    Just (motionCamera model.camera)


focus : Model -> Maybe Focus
focus model =
    Just (appToFocus model.body)
