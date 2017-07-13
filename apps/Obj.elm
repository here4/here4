module Obj exposing
    ( create
    )

import App exposing (..)
import App.Control exposing (..)
import Appearance exposing (Appearance)
import Body exposing (..)
import Dispatch exposing (..)
import Html exposing (Html)
import Math.Vector3 as V3 exposing (vec3)
import Object exposing (..)
import Object.Attributes exposing (..)
import Object.Types exposing (Load(..))
import Orientation


type alias Model vehicle =
    { motion : Moving {}
    , action : Action vehicle
    , body : Maybe (Moving Body)
    , object : Load ObjectResult
    }


type alias Msg
    = ObjectMsg


defaultAttributes : Attributes vehicle Msg
defaultAttributes =
    { id = ""
    , label = ""
    , position = vec3 0 0 0
    , overlay = Html.text ""
    , object = Invisible ()
    , action = Statue
    }


create : List (Update vehicle Msg) -> (App, Cmd AppMsg)
create updates =
    let
        create_ attributes =
            App.create (init attributes)
                { id = always attributes.id
                , label = always attributes.label
                , update = update attributes.action
                , animate = animate
                , bodies = bodies
                , framing = framing
                , focus = focus
                , overlay = always attributes.overlay
                , reposition = reposition
                }
    in
        create_ (List.foldl (\f attr -> f attr) defaultAttributes updates)


loadBody :
    ( Load ObjectResult, Cmd ObjectMsg )
    -> Model vehicle
    -> ( Model vehicle, Cmd (CtrlMsg Msg) )
loadBody (newObject, newMsg) model =
    let
        mBody =
            case newObject of
                Loading _ ->
                    Nothing
                Ready appear ->
                    Just
                        { anchor = AnchorGround
                        , scale = vec3 1 1 1
                        , position = model.motion.position
                        , orientation = model.motion.orientation
                        , appear = appear
                        , velocity = model.motion.velocity
                       }
    in
        ( { model | object = newObject
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
      loadBody (object, objectCmds)
          { motion =
              { position = attributes.position
              , orientation = Orientation.initial
              , velocity = vec3 0 0 0
              }
          , action = attributes.action
          , body = Nothing
          , object = object
          }


setMotion : Moving {} -> Model vehicle -> Model vehicle
setMotion motion model =
    { model | motion = motion
            , body = Maybe.map (copyMotion motion) model.body
    }


update :
    Action vehicle
    -> CtrlMsg Msg
    -> Model vehicle
    -> ( Model vehicle, Cmd (CtrlMsg Msg) )
update action msg model =
        case msg of
            Self m ->
                loadBody (objectUpdate m model.object) model

            Ctrl (Move dp) ->
                -- ( mapBody (translate dp), Cmd.none)
                ( model, Cmd.none )

            Ctrl (Drive ground inputs) ->
                case action of
                    Statue ->
                        ( model, Cmd.none )

                    Vehicle v ->
                        ( setMotion (v.drive v.vehicle ground inputs model.motion) model
                        , Cmd.none
                        )

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
    Maybe.map toFraming model.body


focus : Model vehicle -> Maybe Focus
focus model =
    Maybe.map appToFocus model.body
