module Wedge exposing (create)

import Html exposing (Html)
import Math.Vector3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)
import App exposing (App, AppMsg, Focus, appToFocus)
import Body exposing (..)
import Camera exposing (..)
import Camera.Util as Camera
import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Ground exposing (Ground)
import Model exposing (Inputs)
import Orientation
import Body.Wedge exposing (wedge)
import Vehicles.DreamBird as DreamBird


type alias Model =
    { body : Moving Body
    }


type alias Msg =
    ()


create : String -> Vec3 -> ( App, Cmd AppMsg )
create label pos =
    App.create (init pos)
        { label = always label
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        }


init : Vec3 -> ( Model, Cmd (CtrlMsg Msg) )
init pos =
    ( { body =
            { anchor = AnchorGround
            , scale = vec3 1 1 1
            , position = pos
            , orientation = Orientation.initial
            , appear = wedge
            , velocity = vec3 0 0 0
            }
      }
    , Cmd.none
    )


update :
    CtrlMsg Msg
    -> Model
    -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    let
        mapBody f =
            { model | body = f model.body }
    in
        case msg of
            Ctrl (Control.Move dp) ->
                ( model, Cmd.none )

            Ctrl (Control.Drive ground inputs) ->
                ( mapBody (DreamBird.drive ground inputs), Cmd.none )

            _ ->
                ( model, Cmd.none )


animate : ground -> Time -> Model -> Model
animate ground dt model =
    model


bodies : Model -> List Body
bodies model =
    [ toBody model.body ]


framing : Model -> Maybe Framing
framing model =
    Just (Camera.framing model.body)


focus : Model -> Maybe Focus
focus model =
    Just (appToFocus model.body)

overlay : Model -> Html msg
overlay _ = Html.text "An alien spacecraft"
