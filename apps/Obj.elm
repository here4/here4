module Obj exposing (create)

import App exposing (..)
import App.Control exposing (..)
import Appearance exposing (Appearance)
import Body exposing (..)
import Body.Obj exposing (textured)
import Dict exposing (Dict)
import Dispatch exposing (..)
import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Model exposing (Inputs)
import Orientation
import OBJ
import OBJ.Types exposing (ObjFile, Mesh(..))
import Task exposing (Task)
import Tuple exposing (first)
import Vehicle exposing (Driveable)
import WebGL.Texture as Texture exposing (Texture, Error)


type alias TexturedObjAttributes =
    { meshPath : String
    , diffuseTexturePath : String
    , normalTexturePath : String
    }

type alias TexturedObjResult =
    { mesh : Result String ObjFile
    , diffTexture : Result String Texture
    , normTexture : Result String Texture
    }

type TexturedObjMsg
    = DiffTextureLoaded (Result String Texture)
    | NormTextureLoaded (Result String Texture)
    | LoadObj String (Result String (Dict String (Dict String Mesh)))


type Object
    = TexturedObj TexturedObjAttributes

type ObjectResult
    = TR TexturedObjResult

type Load result
    = Loading result
    | Ready Appearance


type alias Attributes vehicle =
    { id : String
    , label : String
    , position : Vec3
    , overlay : Html (CtrlMsg Msg)
    -- , object : Object
    , object : TexturedObjAttributes
    , drive : Maybe (Driveable vehicle -> Ground -> Inputs -> Moving {} -> Moving {})
    , vehicle : Driveable vehicle
    }

type alias Model vehicle =
    { motion : Moving {}
    , vehicle : Driveable vehicle
    , body : Maybe (Moving Body)
    -- , objResult : Load ObjResult
    , object : Load TexturedObjResult
    }


type alias Msg
    = TexturedObjMsg

texturedObjectInit : TexturedObjAttributes -> (Load TexturedObjResult, Cmd TexturedObjMsg)
texturedObjectInit attributes =
    ( Loading
          { mesh = Err "Loading ..."
          , diffTexture = Err "Loading texture ..."
          , normTexture = Err "Loading texture ..."
          }
    , Cmd.batch
        [ loadTexture attributes.diffuseTexturePath DiffTextureLoaded
        , loadTexture attributes.normalTexturePath NormTextureLoaded
        , loadModel True attributes.meshPath
        ]
    )

texturedObjectUpdate : TexturedObjMsg -> Load TexturedObjResult -> (Load TexturedObjResult, Cmd TexturedObjMsg)
texturedObjectUpdate msg model =
    let
        loadBody m =
            case ( m.mesh, m.diffTexture, m.normTexture ) of
                ( Ok mesh, Ok diffTexture, Ok normTexture ) ->
                    let
                        appear p =
                            Dict.values mesh
                                |> List.concatMap Dict.values
                                |> List.concatMap (\m -> textured diffTexture normTexture m p)
                    in
                        Ready appear
                _ ->
                    Loading m
    in
        case model of
            Ready appear ->
                ( Ready appear, Cmd.none )

            Loading partial ->
            
                case msg of
                    DiffTextureLoaded textureResult ->
                        ( loadBody { partial | diffTexture = textureResult }, Cmd.none )
                    NormTextureLoaded textureResult ->
                        ( loadBody { partial | normTexture = textureResult }, Cmd.none )
                    LoadObj url meshResult ->
                        ( loadBody { partial | mesh = meshResult }, Cmd.none )



create : Attributes d -> ( App, Cmd AppMsg )
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


init : Attributes vehicle -> ( Model vehicle, Cmd (CtrlMsg Msg) )
init attributes =
    let
        (object, objectCmds) =
            -- case attributes.object of
            --     TexturedObj texObj -> texturedObjectInit texObj
            texturedObjectInit attributes.object
    in
    ( { motion =
          { position = attributes.position
          , orientation = Orientation.initial
          , velocity = vec3 0 0 0
          }
      , vehicle = attributes.vehicle
      , body = Nothing
      , object = object
      }
    , Cmd.map Self objectCmds
    )


loadModel : Bool -> String -> Cmd Msg
loadModel withTangents url =
    OBJ.loadObjFileWith { withTangents = withTangents } url (LoadObj url)


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


setMotion : Moving {} -> Model vehicle -> Model vehicle
setMotion motion model =
    { model | motion = motion
            , body = Maybe.map (copyMotion motion) model.body
    }


update :
    Maybe (Driveable vehicle -> Ground -> Inputs -> Moving {} -> Moving {})
    -> CtrlMsg Msg
    -> Model vehicle
    -> ( Model vehicle, Cmd (CtrlMsg Msg) )
update mDrive msg model =
        case msg of
            Self m ->
                let
                    ( newObject, newMsg ) =
                        texturedObjectUpdate m model.object
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
                    , Cmd.none
                    )

            Ctrl (Move dp) ->
                -- ( mapBody (translate dp), Cmd.none)
                ( model, Cmd.none )

            Ctrl (Drive ground inputs) ->
                case mDrive of
                    Just drive ->
                        -- ( mapBody (drive ground inputs), Cmd.none )
                        ( setMotion (drive model.vehicle ground inputs model.motion) model
                        , Cmd.none
                        )

                    Nothing ->
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
    Maybe.map toFraming model.body


focus : Model vehicle -> Maybe Focus
focus model =
    Maybe.map appToFocus model.body
