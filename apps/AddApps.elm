module AddApps
    exposing
        ( addApps
        )

import Here4.App as App exposing (..)
import Here4.App.Types exposing (..)
import Here4.Appearance exposing (Appearance)
import Here4.Body exposing (..)
import Here4.Bounding exposing (emplace)
import Here4.Bounding.Box exposing (boundingBox)
import Here4.Dispatch exposing (..)
import Here4.Orientation as Orientation
import Here4.Primitive.Cube as Cube
import Here4.Setter exposing (..)
import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (Vec4, vec4)
import Task exposing (Task)
import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)


type alias Model =
    ()


type alias Msg =
    ()


addApps : List ( App, Cmd AppMsg ) -> ( App, Cmd AppMsg )
addApps apps =
    App.createUncontrolled (init apps)
        { id = always ""
        , label = always ""
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = noFraming
        , focus = always Nothing
        , overlay = overlay
        , reposition = always identity
        }


addAppEffect : ( App, Cmd AppMsg ) -> Cmd AppMsg
addAppEffect app =
    Task.succeed app
        |> Task.perform (Effect << AddApp ())

init : List ( App, Cmd AppMsg ) -> ( Model, Cmd AppMsg )
init apps =
    ( ()
    , Cmd.batch (List.map addAppEffect apps)
    )

update : AppMsg -> Model -> ( Model, Cmd AppMsg )
update msg model =
    ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt model =
    model


bodies : Model -> Vec3 -> List Body
bodies model pos =
    []


overlay : Model -> Html msg
overlay _ =
    Html.text ""
