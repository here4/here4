module Obj exposing (create)

import App exposing (..)
import App.Control exposing (..)
import Body exposing (..)
import Body.Obj exposing (textured)
import Dict exposing (Dict)
import Dispatch exposing (..)
import Html exposing (Html)
import Math.Vector3 exposing (Vec3, vec3)
import Model exposing (Inputs)
import Orientation
import OBJ
import OBJ.Types exposing (ObjFile, Mesh(..))
import Task exposing (Task)
import Tuple exposing (first)
import WebGL.Texture as Texture exposing (Texture, Error)


type alias Attributes =
    { id : String
    , label : String
    , position : Vec3
    , overlay : Html (CtrlMsg Msg)
    , meshPath : String
    , diffuseTexturePath : String
    , normalTexturePath : String
    , drive : Maybe (Ground -> Inputs -> Moving {} -> Moving {})
    }


type alias Model =
    { motion : Moving {}
    , body : Maybe (Moving Body)
    , mesh : Result String ObjFile
    , diffTexture : Result String Texture
    , normTexture : Result String Texture
    }


type Msg
    = DiffTextureLoaded (Result String Texture)
    | NormTextureLoaded (Result String Texture)
    | LoadObj String (Result String (Dict String (Dict String Mesh)))


create : Attributes -> ( App, Cmd AppMsg )
create attributes =
    App.create (init attributes)
        { id = always attributes.id
        , label = always attributes.label
        , update = update attributes.drive
        , animate = animate
        , bodies = bodies
        , framing = framing
        , focus = focus
        , overlay = always attributes.overlay
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
      , diffTexture = Err "Loading texture ..."
      , normTexture = Err "Loading texture ..."
      }
    , Cmd.batch
        [ loadTexture attributes.diffuseTexturePath (Self << DiffTextureLoaded)
        , loadTexture attributes.normalTexturePath (Self << NormTextureLoaded)
        , loadModel True attributes.meshPath
        ]
    )


loadModel : Bool -> String -> Cmd (CtrlMsg Msg)
loadModel withTangents url =
    OBJ.loadObjFileWith { withTangents = withTangents } url (Self << LoadObj url)


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
    Maybe (Ground -> Inputs -> Moving {} -> Moving {})
    -> CtrlMsg Msg
    -> Model
    -> ( Model, Cmd (CtrlMsg Msg) )
update mDrive msg model =
    let
        -- mapBody f =
        --     (\m -> { m | body = Maybe.map f m.body }) model

        loadBody m =
            case ( m.mesh, m.diffTexture, m.normTexture ) of
                ( Ok mesh, Ok diffTexture, Ok normTexture ) ->
                    let
                        appear p =
                            Dict.values mesh
                                |> List.concatMap Dict.values
                                |> List.concatMap (\m -> textured diffTexture normTexture m p)
                    in
                        { m
                            | body =
                                Just
                                    { anchor = AnchorGround
                                    , scale = vec3 1 1 1
                                    , position = model.motion.position
                                    , orientation = model.motion.orientation
                                    , appear = appear
                                    , velocity = model.motion.velocity
                                    }
                        }

                _ ->
                    m
    in
        case msg of
            Self (DiffTextureLoaded textureResult) ->
                ( loadBody { model | diffTexture = textureResult }, Cmd.none )

            Self (NormTextureLoaded textureResult) ->
                ( loadBody { model | normTexture = textureResult }, Cmd.none )

            Self (LoadObj url meshResult) ->
                ( loadBody { model | mesh = meshResult }, Cmd.none )

            Ctrl (Move dp) ->
                -- ( mapBody (translate dp), Cmd.none)
                ( model, Cmd.none )

            Ctrl (Drive ground inputs) ->
                case mDrive of
                    Just drive ->
                        -- ( mapBody (drive ground inputs), Cmd.none )
                        ( setMotion (drive ground inputs model.motion) model
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

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
