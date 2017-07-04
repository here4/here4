module StaticGround exposing (create)

import Html exposing (Html)
import Task exposing (Task)
import Time exposing (Time)
import App exposing (App, AppMsg, Focus, noFraming)
import Body exposing (Body)
import Control exposing (..)
import Dispatch exposing (..)
import Ground exposing (Ground)
import Math.Vector3 exposing (vec3)


type alias Model =
    List Body


type Msg
    = GroundGenerated ( Ground, List Body )


create :
    ((( Ground, List Body ) -> Msg) -> Cmd Msg)
    -> ( App, Cmd AppMsg )
create makeGround =
    App.create (init makeGround)
        { label = always "Ground"
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = noFraming
        , focus = always Nothing
        , overlay = overlay
        , getPosition = always (vec3 0 0 0)
        , setPosition = always identity
        }


init :
    ((( Ground, List Body ) -> Msg) -> Cmd Msg)
    -> ( Model, Cmd (CtrlMsg Msg) )
init makeGround =
    ( []
    , Cmd.map Self (makeGround GroundGenerated)
    )


update :
    CtrlMsg Msg
    -> Model
    -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    case msg of
        Self (GroundGenerated ( ground, bodies )) ->
            ( bodies
            , Task.succeed ground
                |> Task.perform (Effect << UpdateGround)
            )

        _ ->
            ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt model =
    model


bodies : Model -> List Body
bodies =
    identity

overlay : Model -> Html msg
overlay _ = Html.text "Generated terrain"
