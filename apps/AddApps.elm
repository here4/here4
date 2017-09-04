module AddApps
    exposing
        ( addApps
        , addRandom
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
import Random
import Task exposing (Task)
import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)


type alias Model =
    Maybe (Vec3 -> ( App, Cmd AppMsg ))


type Msg
    = AppGenerated ( App, Cmd AppMsg )


methods =
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


addApps : List ( App, Cmd AppMsg ) -> ( App, Cmd AppMsg )
addApps apps =
    App.create
        ( Nothing , Cmd.batch (removeSelf :: List.map addAppEffect apps) )
        methods


addRandom : Random.Generator ( App, Cmd AppMsg ) -> ( App, Cmd AppMsg )
addRandom gen =
    App.create
        ( Nothing, Cmd.map Self (Random.generate AppGenerated gen) )
        methods

{-
addAnywhere : (Vec3 -> (App, Cmd AppMsg) ) -> ( App, Cmd AppMsg )
addAnywhere placer =
    App.create
        ( Just placer, Cmd.map Self (Random.generate PosGenerated randomPosition
        methods
-}

addAppEffect : ( App, Cmd AppMsg ) -> Cmd (CtrlMsg msg)
addAppEffect app =
    Task.succeed app
        |> Task.perform (Effect << AddApp ())


removeSelf : Cmd (CtrlMsg msg)
removeSelf =
    Task.succeed ()
        |> Task.perform (Effect << RemoveApp ())


update : CtrlMsg Msg -> Model -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    case msg of
        Self (AppGenerated app) ->
            ( model
            , Cmd.batch [ removeSelf, addAppEffect app ]
            )
        _ ->
            ( model, Cmd.none )


animate : Ground -> Time -> Model -> ( Model, Cmd (CtrlMsg Msg) )
animate ground dt model =
    ( model, Cmd.none )


bodies : Model -> Vec3 -> List Body
bodies model pos =
    []


overlay : Model -> Html msg
overlay _ =
    Html.text ""
