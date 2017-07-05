module TextureCube exposing (create)

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
import Primitive.Cube exposing (textureCube)
import Vehicles.DreamBuggy as DreamBuggy


type alias Model =
    Maybe { body : Moving Body
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
        , reposition = reposition
        }


init : String -> ( Model, Cmd (CtrlMsg Msg) )
init path =
    ( Nothing
    , Texture.load path
        |> Task.attempt (Self << TextureLoaded)
    )


update :
    CtrlMsg Msg
    -> Model
    -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    let mapBody f =
            Maybe.map (\m -> { m | body = f m.body } ) model
    in
    case msg of
        Self (TextureLoaded textureResult) ->
            case textureResult of
                Ok texture ->
                    let
                        body =
                            { anchor = AnchorGround
                            , scale = vec3 1 1 1
                            , position = vec3 -2 0 -17
                            , orientation = Orientation.initial
                            , appear = textureCube texture
                            , velocity = vec3 0 0 0
                            }
                    in
                        ( Just { body = body }, Cmd.none )

                Err msg ->
                    -- ( { model | message = "Error loading texture" }, Cmd.none )
                    ( model, Cmd.none )
        Ctrl (Control.Move dp) ->
            -- ( mapBody (translate dp), Cmd.none)
            ( model, Cmd.none )

        Ctrl (Control.Drive ground inputs) ->
            ( mapBody (DreamBuggy.drive { speed = 8.0 } ground inputs), Cmd.none )
            -- ( Maybe.map (\m -> { m | body = DreamBuggy.drive ground 8.0 inputs) m.body } model, Cmd.none )

        _ ->
            ( model, Cmd.none )


animate : ground -> Time -> Model -> Model
animate ground dt model =
    model


bodies : Model -> List Body
bodies model_ =
    case model_ of
        Just model ->
            [ toBody model.body ]

        Nothing ->
            []


reposition : Maybe AppPosition -> Model -> Model
reposition mPos model =
    let mapBody f =
            Maybe.map (\m -> { m | body = f m.body } ) model
        setPos pos body = { body | position = pos.position, orientation = pos.orientation }
    in
        case mPos of
            Just pos ->
                mapBody (setPos pos)
            Nothing ->
                model


framing : Model -> Maybe Framing
framing model_ =
    case model_ of
        Just model ->
            Just (Camera.framing model.body)

        Nothing ->
            Nothing
    

focus : Model -> Maybe Focus
focus model =
    Maybe.map (.body >> appToFocus) model

overlay : Model -> Html msg
overlay _ =
    Html.div []
        [ Html.h2 []
              [ Html.text "A wooden box" ]
        , Html.text "This highly attractive wooden box doubles as a secret vehicle."
        , Html.br [] []
        , Html.hr [] []
        , DreamBuggy.overlay
        ]
