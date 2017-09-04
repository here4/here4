module StaticGround exposing (create)

import Here4.App as App exposing (..)
import Here4.App.Types exposing (..)
import Here4.Body exposing (Body)
import Here4.Dispatch exposing (..)
import Html exposing (Html)
import Math.Vector3 exposing (Vec3, vec3)
import Task exposing (Task)


type alias Model =
    List Body


type Msg
    = GroundGenerated ( Ground, List Body )


create :
    ((( Ground, List Body ) -> Msg) -> Cmd Msg)
    -> ( App, Cmd AppMsg )
create makeGround =
    App.create (init makeGround)
        { id = always "ground"
        , label = always "Ground"
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = noFraming
        , focus = always Nothing
        , overlay = overlay
        , reposition = always identity
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
                |> Task.perform (Effect << UpdateGround ())
            )

        _ ->
            ( model, Cmd.none )


animate : Ground -> Time -> Model -> ( Model, Cmd (CtrlMsg Msg) )
animate ground dt model =
    ( model, Cmd.none )


bodies : Model -> Vec3 -> List Body
bodies model pos =
    model


overlay : Model -> Html msg
overlay _ =
    Html.text "Generated terrain"
