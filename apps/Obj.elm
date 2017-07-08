module Obj exposing (create)

import Html exposing (Html)
import Math.Vector3 exposing (Vec3, vec3)
import Dict exposing (Dict)
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
import Model exposing (Inputs, PartyKey)
import Orientation
import Body.Obj exposing (obj2)
import OBJ
import OBJ.Types exposing (ObjFile, Mesh(..))


type alias Attributes =
    { id : String
    , label : String
    , overlay : Html (CtrlMsg Msg)
    , meshPath : String
    , diffuseTexturePath : String
    , normalTexturePath : String
    , drive : Maybe (Ground -> Inputs -> Moving Body -> Moving Body)
    }


type alias Model =
    { body : Maybe (Moving Body)
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
    ( { body = Nothing
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


update :
    Maybe (Ground -> Inputs -> Moving Body -> Moving Body)
    -> CtrlMsg Msg
    -> Model
    -> ( Model, Cmd (CtrlMsg Msg) )
update mDrive msg model =
    let
        mapBody f =
            (\m -> { m | body = Maybe.map f m.body }) model

        loadBody m =
            case ( m.mesh, m.diffTexture, m.normTexture ) of
                ( Ok mesh, Ok diffTexture, Ok normTexture ) ->
                    let
                        appear p =
                            Dict.values mesh
                                |> List.concatMap Dict.values
                                |> List.concatMap (\m -> obj2 diffTexture normTexture m p)
                    in
                        { m
                            | body =
                                Just
                                    { anchor = AnchorGround
                                    , scale = vec3 1 1 1
                                    , position = vec3 38 0 12
                                    , orientation = Orientation.initial
                                    , appear = appear
                                    , velocity = vec3 0 0 0
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

            Ctrl (Control.Move dp) ->
                -- ( mapBody (translate dp), Cmd.none)
                ( model, Cmd.none )

            Ctrl (Control.Drive ground inputs) ->
                case mDrive of
                    Just drive ->
                        ( mapBody (drive ground inputs), Cmd.none )

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
    let
        mapBody f =
            (\m -> { m | body = Maybe.map f m.body }) model

        setPos body =
            case mPos of
                Just pos ->
                    { body | position = pos.position, orientation = pos.orientation }

                Nothing ->
                    body
    in
        mapBody setPos


framing : PartyKey -> Model -> Maybe Framing
framing _ model =
    Maybe.map (Camera.framing) model.body


focus : Model -> Maybe Focus
focus model =
    Maybe.map appToFocus model.body
