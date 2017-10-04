module BoxRoom
    exposing
        ( create
        , floor
        , dimensions
        , color
        , textureScale
        , timeScale
        , smoothing
        )

import Here4.App as App exposing (..)
import Here4.App.Types exposing (..)
import Here4.Appearance exposing (Appearance)
import Here4.Body exposing (..)
import Here4.Bounding exposing (emplace)
import Here4.Bounding.Box exposing (boundingBox)
import Here4.Dispatch exposing (..)
import Here4.Ground as Ground exposing (barrierFromQuads, Quad(..))
import Here4.Orientation as Orientation
import Here4.Primitive.Cube as Cube
import Here4.Setter exposing (..)
import Html exposing (Html)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (Vec4, vec4)
import Task exposing (Task)
import Shaders.WorldVertex exposing (worldVertex)
import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)
import Shaders.Mondrian exposing (mondrian)
import Shaders.PointLightSquares exposing (pointLightSquares)


type alias Attributes =
    { id : String
    , label : String
    , floor : Float
    , dimensions : Vec3
    , color : Vec4
    , textureScale : Float
    , timeScale : Float
    , smoothing : Float
    }


floor : Float -> Update Attributes
floor f attr =
    { attr | floor = f }


dimensions : Vec3 -> Update Attributes
dimensions dim attr =
    { attr | dimensions = dim }


color : Vec4 -> Update Attributes
color c attr =
    { attr | color = c }


textureScale : Float -> Update Attributes
textureScale s attr =
    { attr | textureScale = s }


timeScale : Float -> Update Attributes
timeScale s attr =
    { attr | timeScale = s }


smoothing : Float -> Update Attributes
smoothing s attr =
    { attr | smoothing = s }


defaultAttributes : Attributes
defaultAttributes =
    { id = "_room"
    , label = "Room"
    , floor = 0.0
    , dimensions = vec3 10 3 10
    , color = vec4 1.0 1.0 1.0 1.0
    , textureScale = 0.1
    , timeScale = 0.0
    , smoothing = 0.3
    }


type alias Model =
    { walls : Body
    , floor : Body
    , ceiling : Body
    }


type alias Msg =
    ()


create : List (Update Attributes) -> ( App, Cmd AppMsg )
create updates =
    let
        create_ attributes =
            App.create (init attributes)
                { id = always attributes.id
                , label = always attributes.label
                , update = update
                , animate = animate
                , bodies = bodies
                , framing = noFraming
                , focus = always Nothing
                , overlay = overlay
                , reposition = always identity
                }
    in
        create_ (applyUpdates updates defaultAttributes)


init : Attributes -> ( Model, Cmd (CtrlMsg Msg) )
init attributes =
    let
        ( width, height, length ) =
            V3.toTuple attributes.dimensions

        floorCenterPosition =
            vec3 0 attributes.floor 0

        originPosition =
            vec3 (-width / 2) attributes.floor (-length / 2)

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
            , coord = v.coord
            , color = attributes.color
            , textureScale = attributes.textureScale
            , timeScale = attributes.timeScale
            , smoothing = attributes.smoothing
            }

        walls =
            -- make (Cube.wallsWith toNoiseVertex noiseVertex noiseColorFragment)
            make (Cube.walls worldVertex mondrian)

        floor =
            make (Cube.floorWith toNoiseVertex noiseVertex noiseColorFragment)

        ceiling =
            -- make (Cube.ceilingWith toNoiseVertex noiseVertex noiseColorFragment)
            make (Cube.ceiling worldVertex pointLightSquares)

        box =
            { position = originPosition
            , dimensions = attributes.dimensions
            }

        barrier =
            let
                x0 = V3.getX originPosition
                x1 = x0 + width
                y0 = V3.getY originPosition
                y1 = y0 + height
                z0 = V3.getZ originPosition
                z1 = z0 + length

                fSW = vec3 x0 y0 z0
                fSE = vec3 x1 y0 z0
                fNW = vec3 x0 y0 z1
                fNE = vec3 x1 y0 z1
                cSW = vec3 x0 y1 z0
                cSE = vec3 x1 y1 z0
                cNW = vec3 x0 y1 z1
                cNE = vec3 x1 y1 z1
            in
                barrierFromQuads Ground.Grass
                    [ Quad fSW fSE fNE fNW
                    , Quad fSW fNW cNW cSW
                    , Quad fNW fNE cNE cNW
                    , Quad fNE fSE cSE cNE
                    , Quad fSE fSW cSW cSE
                    , Quad cNW cNE cSE cSW
                    ]

        ground =
            { bounds = emplace (boundingBox box)
            , elevation = always attributes.floor
            , barrier = barrier
            , seaLevel = 0.0
            , surface = always Ground.Grass
            , coordRangeX =
                let
                    minX =
                        V3.getX originPosition
                in
                    ( minX, minX + width )
            , coordRangeZ =
                let
                    minZ =
                        V3.getZ originPosition
                in
                    ( minZ, minZ + length )
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


animate : Ground -> Time -> Model -> ( Model, Cmd (CtrlMsg Msg) )
animate ground dt model =
    ( model, Cmd.none )


bodies : Model -> Vec3 -> List Body
bodies model pos =
    [ model.walls, model.floor, model.ceiling ]


overlay : Model -> Html msg
overlay _ =
    Html.text "The room"
