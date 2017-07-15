module Obj exposing
    ( create
    )

import App exposing (..)
import App.Control exposing (..)
import Appearance exposing (Appearance)
import Body exposing (..)
import Dispatch exposing (..)
import Html exposing (Html)
import Html.Attributes as Html
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Object exposing (..)
import Object.Attributes exposing (..)
import Object.Types exposing (Load(..))
import Orientation exposing (Orientation)


type alias Model vehicle =
    { motion : Moving {}
    , rotation : Maybe Orientation
    , action : Action vehicle
    , body : Maybe (Moving Body)
    , object : Load ObjectResult
    }


type alias Msg
    = ObjectMsg



create : List (Update vehicle Msg) -> (App, Cmd AppMsg)
create updates =
    let
        create_ attributes =
            App.create (init attributes)
                { id = always attributes.id
                , label = always attributes.label
                , update = update attributes.scale attributes.action
                , animate = animate
                , bodies = bodies
                , framing = framing
                , focus = focus
                , overlay = always attributes.overlay
                , reposition = reposition
                }
    in
        create_ (List.foldl (\f attr -> f attr) defaultAttributes updates)


applyMotion : Model vehicle -> Model vehicle
applyMotion model =
    let
        orientation o =
            model.rotation
            |> Maybe.map (Orientation.unwind o)
            |> Maybe.withDefault o

        apply motion thing =
            { thing | position = motion.position
                    , orientation = orientation motion.orientation
                    , velocity = motion.velocity
            }
    in
        { model | body = Maybe.map (apply model.motion) model.body }


setMotion : Moving {} -> Model vehicle -> Model vehicle
setMotion motion model = applyMotion { model | motion = motion }


loadBody :
    Float
    -> ( Load ObjectResult, Cmd ObjectMsg )
    -> Model vehicle
    -> ( Model vehicle, Cmd (CtrlMsg Msg) )
loadBody s (newObject, newMsg) model =
    let
        mBody =
            case newObject of
                Loading _ ->
                    Nothing
                Ready appear ->
                    Just
                        { anchor = AnchorGround
                        , scale = vec3 s s s
                        , position = vec3 0 0 0
                        , orientation = Orientation.initial
                        , appear = appear
                        , velocity = vec3 0 0 0
                       }
    in
        ( applyMotion
              { model | object = newObject
                      , body = mBody
              }
        , Cmd.map Self newMsg
        )


init : Attributes vehicle Msg -> ( Model vehicle, Cmd (CtrlMsg Msg) )
init attributes =
    let
        (object, objectCmds) =
            objectInit attributes.object
    in
      loadBody attributes.scale (object, objectCmds)
          { motion =
              { position = attributes.position
              , orientation = Orientation.initial
              , velocity = vec3 0 0 0
              }
          , rotation = attributes.rotation
          , action = attributes.action
          , body = Nothing
          , object = object
          }


update :
    Float
    -> Action vehicle
    -> CtrlMsg Msg
    -> Model vehicle
    -> ( Model vehicle, Cmd (CtrlMsg Msg) )
update scale action msg model =
        case msg of
            Self m ->
                loadBody scale (objectUpdate m model.object) model

            Ctrl (Enter partyKey) ->
                case action of
                    Portal location ->
                        ( model, teleport partyKey location )

                    _ ->
                        ( model, Cmd.none )

            Ctrl (Move dp) ->
                -- ( mapBody (translate dp), Cmd.none)
                ( model, Cmd.none )

            Ctrl (Drive ground inputs) ->
                case action of
                    Vehicle v ->
                        ( setMotion (v.drive v.vehicle ground inputs model.motion) model
                        , Cmd.none
                        )

                    _ ->
                        ( model, Cmd.none )


            _ ->
                ( model, Cmd.none )


animate : Ground -> Time -> Model vehicle -> Model vehicle
animate ground dt model =
    let
        aboveGround pos =
            let
                minY = 1.8 + ground.elevation pos
            in
                if V3.getY pos > minY then
                    pos
                else
                    V3.setY minY pos

        motion = model.motion

        newMotion =
            { motion | position = aboveGround motion.position
            }
    in
        setMotion newMotion model


bodies : Model vehicle -> List Body
bodies model_ =
    case model_.body of
        Just body ->
            [ toBody body ]

        Nothing ->
            []


reposition : Maybe AppPosition -> Model vehicle -> Model vehicle
reposition mPos model =
    case mPos of
        Just pos ->
            let
                motion = model.motion
                newMotion =
                    { motion | position = pos.position
                             , orientation = pos.orientation
                    }
            in
                setMotion newMotion model

        Nothing ->
            model


framing : PartyKey -> Model vehicle -> Maybe Framing
framing _ model =
    Maybe.map (always (toFraming model.motion)) model.body


focus : Model vehicle -> Maybe Focus
focus model =
    Maybe.map appToFocus model.body

{-
overlay : Model vehicle -> Html msg
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
-}
