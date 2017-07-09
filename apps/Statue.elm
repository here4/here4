module Statue exposing (create, portal)

import App exposing (..)
import App.Control exposing (..)
import Appearance exposing (Appearance)
import Body exposing (..)
import Dispatch exposing (..)
import Html exposing (Html)
import Html.Attributes as Html
import Location exposing (..)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Orientation


type alias Attributes =
    { id : String
    , label : String
    , position : Vec3
    , appear : Appearance
    }


type alias Model =
    { label : String
    , body : Moving Body
    , destination : Maybe Location
    }


type alias Msg =
    ()


create : Attributes -> ( App, Cmd AppMsg )
create =
    createHelp Nothing


portal : Location -> Attributes -> ( App, Cmd AppMsg )
portal location =
    createHelp (Just location)


createHelp : Maybe Location -> Attributes -> ( App, Cmd AppMsg )
createHelp destination attributes =
    App.create (init destination attributes)
        { id = always attributes.id
        , label = always attributes.label
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        , reposition = reposition
        }


init : Maybe Location -> Attributes -> ( Model, Cmd (CtrlMsg Msg) )
init destination attributes =
    ( { label = attributes.label
      , body =
            { anchor = AnchorGround
            , scale = vec3 1 1 1
            , position = attributes.position
            , orientation = Orientation.initial
            , appear = attributes.appear
            , velocity = vec3 0 0 0
            }
      , destination = destination
      }
    , Cmd.none
    )


update : CtrlMsg Msg -> Model -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    case msg of
        Ctrl (Enter partyKey) ->
            case model.destination of
                Just dest ->
                    ( model, teleport partyKey dest )

                Nothing ->
                    ( model, Cmd.none )

        Ctrl (Move dp) ->
            ( { model | body = translate dp model.body }, Cmd.none )

        _ ->
            ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt model =
    let
        setElevation pos =
            V3.setY (1.8 + ground.elevation pos) pos

        onGround body =
            { body | position = setElevation body.position }
    in
        { model | body = onGround model.body }


bodies : Model -> List Body
bodies model =
    [ toBody model.body ]


reposition : Maybe AppPosition -> Model -> Model
reposition mPos model =
    let
        setPos pos x =
            { x | position = pos.position, orientation = pos.orientation }

        behind pos =
            let
                dir =
                    Orientation.rotateLabV pos.orientation V3.k
            in
                { pos | position = V3.sub pos.position dir }
    in
        case mPos of
            Just pos ->
                { model
                    | body = setPos pos model.body
                }

            Nothing ->
                model


framing : PartyKey -> Model -> Maybe Framing
framing _ model =
    Just (toFraming model.body)


focus : Model -> Maybe Focus
focus model =
    Just (appToFocus model.body)


overlay : Model -> Html msg
overlay model =
    let
        textLeft =
            Html.style [ ( "text-align", "left" ) ]
    in
        Html.div []
            [ Html.h2 []
                [ Html.text model.label ]
            , Html.text "A statue"
            ]
