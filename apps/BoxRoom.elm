module BoxRoom exposing (create)

import Here4.App as App exposing (..)
import Here4.App.Control exposing (..)
import Here4.Appearance exposing (Appearance)
import Here4.Body exposing (..)
import Here4.Bounding exposing (emplace)
import Here4.Bounding.Box exposing (boundingBox)
import Here4.Dispatch exposing (..)
import Here4.Orientation as Orientation
import Here4.Primitive.Cube as Cube
import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (vec4)
import Task exposing (Task)
import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)


type alias Attributes =
    { dimensions : Vec3
    }


type alias Model =
    { walls : Body
    , floor : Body
    , ceiling : Body
    }


type alias Msg =
    ()


create : Attributes -> ( App, Cmd AppMsg )
create attributes =
    App.create (init attributes)
        { id = always "_room"
        , label = always "Box_Room"
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = noFraming
        , focus = always Nothing
        , overlay = overlay
        , reposition = always identity
        }


init : Attributes -> ( Model, Cmd (CtrlMsg Msg) )
init attributes =
    let
        ( width, height, length ) =
            V3.toTuple attributes.dimensions

        floorCenterPosition =
            vec3 0 0 0

        originPosition =
            vec3 (-width / 2) 0 (-length / 2)

        make appear =
            { anchor = AnchorGround
            , scale = attributes.dimensions
            , position = floorCenterPosition
            , orientation = Orientation.initial
            , appear = appear
            }

        toNoiseVertex v =
            { position = v.position
            , normal = v.normal
            , coord = V3.scale 1.0 v.coord
            , color = vec4 1.0 1.0 1.0 1.0
            , smoothing = 0.3
            , textureScale = 0.1
            , timeScale = 0.0
            }

        walls =
            make (Cube.wallsWith toNoiseVertex noiseVertex noiseColorFragment)

        floor =
            make (Cube.floorWith toNoiseVertex noiseVertex noiseColorFragment)

        ceiling =
            make (Cube.ceilingWith toNoiseVertex noiseVertex noiseColorFragment)

        box =
            { position = originPosition
            , dimensions = attributes.dimensions
            }

        ground =
            { bounds = emplace (boundingBox box)
            , elevation = always 0.0
            }

        groundEffect =
            Task.succeed ground
                |> Task.perform (Effect << UpdateGround ())
    in
        ( { walls = walls
          , floor = floor
          , ceiling = ceiling
          }
        , groundEffect
        )


update : CtrlMsg Msg -> Model -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    ( model, Cmd.none )


animate : Ground -> Time -> Model -> Model
animate ground dt model =
    model


bodies : Model -> Vec3 -> List Body
bodies model pos =
    [ model.walls, model.floor, model.ceiling ]


overlay : Model -> Html msg
overlay _ =
    Html.text "The room"
