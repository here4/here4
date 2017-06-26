module Shufflepuck exposing (create)

import Html exposing (Html)
import Math.Vector3 exposing (Vec3, vec3)
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
import Orientation
import Body.Cube exposing (textureCube)


type alias Model =
    { table : Maybe Body
    }


type Msg
    = TextureLoaded (Result Error Texture)


create : String -> String -> ( App, Cmd AppMsg )
create label path =
    App.create (init path)
        { label = always label
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        }


init : String -> ( Model, Cmd (CtrlMsg Msg) )
init path =
    ( { table = Nothing
      }
    , Texture.load path
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
                        table =
                            { anchor = AnchorGround
                            , scale = vec3 1 0.1 3
                            , position = vec3 56 0 -35
                            , orientation = Orientation.initial
                            , appear = textureCube texture
                            , velocity = vec3 0 0 0
                            }
                    in
                        ( { table = Just table }, Cmd.none )

                Err msg ->
                    -- ( { model | message = "Error loading texture" }, Cmd.none )
                    ( model, Cmd.none )

        Ctrl (Control.Move dp) ->
            ( model, Cmd.none )

        Ctrl (Control.Drive ground inputs) ->
            ( model, Cmd.none )

        Effect _ ->
            ( model, Cmd.none )


animate : ground -> Time -> Model -> Model
animate ground dt model =
    model


bodies : Model -> List Body
bodies model =
    case model.table_ of
        Just table ->
            [ toBody table ]

        Nothing ->
            []
framing : Model -> Maybe Framing
framing model =
        Maybe.map Camera.framing model.table
    

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
