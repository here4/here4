module Wedge exposing (create)

import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Task exposing (Task)
import App exposing (..)
import Body exposing (..)
import Control
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
        { id = always "wedge"
        , label = always label
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        , reposition = reposition
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
                ( mapBody (DreamBird.drive { speed = 20.0 } ground inputs), Cmd.none )

            _ ->
                ( model, Cmd.none )


animate : ground -> Time -> Model -> Model
animate ground dt model =
    model


bodies : Model -> List Body
bodies model =
    [ toBody model.body ]


reposition : Maybe AppPosition -> Model -> Model
reposition mPos model =
    let
        setPos pos body =
            { body | position = pos.position, orientation = pos.orientation }
    in
        case mPos of
            Just pos ->
                { model | body = setPos pos model.body }

            Nothing ->
                model


framing : PartyKey -> Model -> Maybe Framing
framing partyKey model =
    let
        framing =
            toFraming model.body

        pov0 =
            framing.pov

        position =
            V3.add model.body.position <|
                V3.scale 1.0
                    (Orientation.rotateLabV model.body.orientation V3.k)

        pov =
            { pov0 | position = position }
    in
        Just { framing | pov = pov }


focus : Model -> Maybe Focus
focus model =
    Just (appToFocus model.body)


overlay : Model -> Html msg
overlay _ =
    Html.div []
        [ Html.h2 []
            [ Html.text "Delta Wedge" ]
        , Html.text "A manoueverable delta wing with solid plasma anti-gravity thrusters and zero-friction flight surfaces."
        , Html.br [] []
        , Html.hr [] []
        , DreamBird.overlay
        ]
