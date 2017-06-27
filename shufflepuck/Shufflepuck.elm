module Shufflepuck exposing (create, default)

import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)
import Tuple exposing (first)
import WebGL.Texture as Texture exposing (Texture, Error)
import App exposing (App, AppMsg, Focus, appToFocus)
import Body exposing (..)
import Camera exposing (..)
import Camera.Util as Camera
import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Ground exposing (Ground)
import Model exposing (Inputs)
import Orientation exposing (Orientation)
import Body.Cube exposing (textureCube)
import Body.Sphere exposing (cloudsSphere, fogMountainsSphere)


type alias Attributes =
    { label : String
    , position : Vec3
    , scale : Vec3
    , orientation : Orientation
    , tableTexture : String
    , tableWidth : Float
    , tableLength : Float
    , tableThickness : Float
    , tableHeight : Float
    , puckRadius : Float
    , puckThickness : Float
    , paddleRadius : Float
    , paddleThickness : Float
    }

default : Attributes
default =
    { label = "Shufflepuck"
    , position = vec3 0 0 0
    , scale = vec3 1 1 1
    , orientation = Orientation.initial
    , tableTexture = "resources/woodCrate.jpg"
    , tableWidth = 3
    , tableLength = 7
    , tableHeight = 0.9
    , tableThickness = 0.1
    , puckRadius = 0.12
    , puckThickness = 0.3
    , paddleRadius = 0.18
    , paddleThickness = 0.7
    }

type alias Model =
    { attributes : Attributes
    , table : Maybe Body
    , puck : Moving Body
    , paddle1 : Moving Body
    , paddle2 : Moving Body
    }


type Msg
    = TextureLoaded (Result Error Texture)


create : Attributes -> ( App, Cmd AppMsg )
create attributes =
    App.create (init attributes)
        { label = always attributes.label
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        }

reposition : Vec3 -> Model -> Model
reposition pos0 model =
    let
        a = model.attributes
        puckY = (a.tableThickness + a.puckThickness) / 2.0
        paddleY = (a.tableThickness + a.paddleThickness) / 2.0
        paddleZ = a.tableLength / 4.0
        setPos pos body = { body | position = pos }
    in
        { model | table = Maybe.map (setPos pos0) model.table
                , puck = setPos (V3.add pos0 (vec3 0 puckY 0)) model.puck
                , paddle1 = setPos (V3.add pos0 (vec3 0 paddleY -paddleZ)) model.paddle1
                , paddle2 = setPos (V3.add pos0 (vec3 0 paddleY paddleZ)) model.paddle2
        } 

init : Attributes -> ( Model, Cmd (CtrlMsg Msg) )
init a =
    ( reposition a.position
        { attributes = a
        , table = Nothing
        , puck =
              { anchor = AnchorGround
              , scale = vec3 a.puckRadius (a.puckThickness/2.0) a.puckRadius
              , position = vec3 0 0 0
              , orientation = Orientation.initial
              , appear = cloudsSphere
              , velocity = vec3 0 0 0
              }
        , paddle1 =
              { anchor = AnchorGround
              , scale = vec3 a.paddleRadius (a.paddleThickness/2.0) a.paddleRadius
              , position = vec3 0 0 0
              , orientation = Orientation.initial
              , appear = cloudsSphere
              , velocity = vec3 0 0 0
              }
        , paddle2 =
              { anchor = AnchorGround
              , scale = vec3 a.paddleRadius (a.paddleThickness/2.0) a.paddleRadius
              , position = vec3 0 0 0
              , orientation = Orientation.initial
              , appear = cloudsSphere
              , velocity = vec3 0 0 0
              }
        }
    , Texture.load a.tableTexture
        |> Task.attempt (Self << TextureLoaded)
    )


update :
    CtrlMsg Msg
    -> Model
    -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    case msg of
        Self (TextureLoaded textureResult) ->
            case textureResult of
                Ok texture ->
                    let
                        a = model.attributes
                        table =
                            { anchor = AnchorGround
                            , scale = vec3 a.tableWidth a.tableThickness a.tableLength
                            , position = a.position
                            , orientation = Orientation.initial
                            , appear = textureCube texture
                            }
                    in
                        ( { model | table = Just table }, Cmd.none )

                Err msg ->
                    -- ( { model | message = "Error loading texture" }, Cmd.none )
                    ( model, Cmd.none )

        Ctrl (Control.Move dp) ->
            ( model, Cmd.none )

        Ctrl (Control.Drive ground inputs) ->
            ( model, Cmd.none )

        Effect _ ->
            ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt model =
    let
        setElevation pos = V3.setY (model.attributes.tableHeight + ground.elevation pos) pos
    in
        reposition (setElevation model.attributes.position) model


bodies : Model -> List Body
bodies model =
    let
        ps = List.map toBody [ model.puck, model.paddle1, model.paddle2 ]
    in
        case model.table of
            Just t ->
                t :: ps

            Nothing ->
                ps


framing : Model -> Maybe Framing
framing model =
    let
        target =
            { position = model.attributes.position
            , orientation = model.attributes.orientation
            , velocity = vec3 0 0 0
            }
    in
        Just { target = target }
    

focus : Model -> Maybe Focus
focus model =
    Maybe.map appToFocus model.table

overlay : Model -> Html msg
overlay _ =
    Html.div []
        [ Html.h2 []
              [ Html.text "Shufflepuck" ]
        , Html.text "An air hockey game."
        , Html.br [] []
        , Html.hr [] []
        ]
