module Suzanne exposing (create)

import App exposing (..)
import App.Control exposing (..)
import Body exposing (..)
import Body.Obj exposing (obj)
import Dispatch exposing (..)
import Html exposing (Html)
import Math.Vector3 exposing (Vec3, vec3)
import Model exposing (Inputs)
import Orientation
import OBJ
import OBJ.Types exposing (MeshWith, VertexWithTexture)
import Task exposing (Task)
import Tuple exposing (first)
import Vehicles.Walking as Walking
import WebGL.Texture as Texture exposing (Texture, Error)

type alias Attributes =
    { id : String
    , label : String
    , position : Vec3
    , height : Float
    , speed : Float
    }


type alias Model =
    { motion : Moving {}
    , body : Maybe (Moving Body)
    , mesh : Result String (MeshWith VertexWithTexture)
    , reflectionTexture : Result String Texture
    }


type Msg
    = TextureLoaded (Result String Texture)
    | LoadObj (Result String (MeshWith VertexWithTexture))


create : Attributes -> ( App, Cmd AppMsg )
create attributes =
    App.create (init attributes)
        { id = always attributes.id
        , label = always attributes.label
        , update = update attributes
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = overlay
        , reposition = reposition
        }


init : Attributes -> ( Model, Cmd (CtrlMsg Msg) )
init attributes =
    ( { motion =
          { position = attributes.position
          , orientation = Orientation.initial
          , velocity = vec3 0 0 0
          }
      , body = Nothing
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


setMotion : Moving {} -> Model -> Model
setMotion motion model =
    { model | motion = motion
            , body = Maybe.map (copyMotion motion) model.body
    }

update :
    Attributes
    -> CtrlMsg Msg
    -> Model
    -> ( Model, Cmd (CtrlMsg Msg) )
update attributes msg model =
    let
        loadBody m =
            case ( m.mesh, m.reflectionTexture ) of
                ( Ok mesh, Ok texture ) ->
                    { m
                        | body =
                            Just
                                { anchor = AnchorGround
                                , scale = vec3 1 1 1
                                , position = model.motion.position
                                , orientation = model.motion.orientation
                                , appear = obj mesh texture
                                , velocity = model.motion.velocity
                                }
                    }

                _ ->
                    m
    in
        case msg of
            Self (TextureLoaded textureResult) ->
                ( loadBody { model | reflectionTexture = textureResult }, Cmd.none )

            Self (LoadObj meshResult) ->
                ( loadBody { model | mesh = meshResult }, Cmd.none )

            Ctrl (Move dp) ->
                -- ( mapBody (translate dp), Cmd.none)
                ( model, Cmd.none )

            Ctrl (Drive ground inputs) ->
                let
                    walkAttributes = { speed = attributes.speed, height = attributes.height, radius = 1.5 }
                in
                    ( setMotion (Walking.drive walkAttributes ground inputs model.motion) model
                    , Cmd.none
                    )
            _ ->
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


framing : PartyKey -> Model -> Maybe Framing
framing _ model =
    Maybe.map toFraming model.body


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
