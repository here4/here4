module Suzanne exposing (create)

import Html exposing (Html)
import Math.Vector3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)
import Tuple exposing (first)
import WebGL.Texture as Texture exposing (Texture, Error)
import App exposing (App, AppMsg, AppPosition, Focus, appToFocus)
import Body exposing (..)
import Camera exposing (..)
import Camera.Util as Camera
import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Ground exposing (Ground)
import Model exposing (Inputs)
import Orientation
import Body.Obj exposing (obj)
import Vehicles.Walking as Walking

import OBJ
import OBJ.Types exposing (MeshWith, VertexWithTexture)

type alias Attributes =
    { label : String
    , height : Float
    , speed : Float
    }

type alias Model =
    { body : Maybe (Moving Body)
    , mesh : Result String (MeshWith VertexWithTexture)
    , reflectionTexture : Result String Texture
    }


type Msg
    = TextureLoaded (Result String Texture)
    | LoadObj (Result String (MeshWith VertexWithTexture))


create : Attributes -> ( App, Cmd AppMsg )
create attributes =
    App.create init
        { label = always attributes.label
        , update = update attributes
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        , reposition = reposition
        }


init : ( Model, Cmd (CtrlMsg Msg) )
init =
    ( { body = Nothing
      , mesh = Err "Loading ..."
      , reflectionTexture = Err "Loading texture ..."
      }
    , Cmd.batch
        [ loadTexture "textures/chavant.jpg" (Self << TextureLoaded)
        , OBJ.loadMesh "meshes/suzanne.obj" (Self << LoadObj)
        ]
    )


loadTexture : String -> (Result String Texture -> msg) -> Cmd msg
loadTexture url msg =
    Texture.load url
        |> Task.attempt
            (\r ->
                case r of
                    Ok t ->
                        msg (Ok t)

                    Err e ->
                        msg (Err ("Failed to load texture: " ++ toString e))
            )

update :
    Attributes
    -> CtrlMsg Msg
    -> Model
    -> ( Model, Cmd (CtrlMsg Msg) )
update attributes msg model =
    let mapBody f =
            (\m -> { m | body = Maybe.map f m.body } ) model

        loadBody m =
            case (m.mesh, m.reflectionTexture) of
                (Ok mesh, Ok texture) ->
                    { m | body = Just
                            { anchor = AnchorGround
                            , scale = vec3 1 1 1
                            , position = vec3 13 0 38
                            , orientation = Orientation.initial
                            , appear = obj mesh texture
                            , velocity = vec3 0 0 0
                            }
                    }
                _ -> m
    in
    case msg of
        Self (TextureLoaded textureResult) ->
            ( loadBody { model | reflectionTexture = textureResult }, Cmd.none )

        Self (LoadObj meshResult) ->
            ( loadBody { model | mesh = meshResult }, Cmd.none )

        Ctrl (Control.Move dp) ->
            -- ( mapBody (translate dp), Cmd.none)
            ( model, Cmd.none )

        Ctrl (Control.Drive ground inputs) ->
            ( mapBody (Walking.drive { speed = attributes.speed, height = attributes.height } ground inputs), Cmd.none )

        Effect _ ->
            ( model, Cmd.none )


animate : ground -> Time -> Model -> Model
animate ground dt model =
    model


bodies : Model -> List Body
bodies model_ =
    case model_.body of
        Just body ->
            [ toBody body ]

        Nothing ->
            []


reposition : Maybe AppPosition -> Model -> Model
reposition mPos model =
    let mapBody f =
            (\m -> { m | body = Maybe.map f m.body } ) model
        setPos pos body = { body | position = pos.position, orientation = pos.orientation }
    in
        case mPos of
            Just pos ->
                mapBody (setPos pos)
            Nothing ->
                model


framing : Model -> Maybe Framing
framing model = Maybe.map (Camera.framing) model.body
    

focus : Model -> Maybe Focus
focus model =
    Maybe.map appToFocus model.body

overlay : Model -> Html msg
overlay _ =
    Html.div []
        [ Html.h2 []
              [ Html.text "Suzanne" ]
        , Html.text "From Blender."
        , Html.br [] []
        , Html.hr [] []
        , Walking.overlay
        ]
