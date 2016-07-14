module View exposing (view)

-- import Html
import Html exposing (Html, text, div, p)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as M4
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (..)
import Random
import Time exposing (Time)
import WebGL
import Window

import Array2D exposing (Array2D)
import Model exposing (Model, Msg)
import Orientation
import Thing exposing (..)
-- import Model
import Things.Cube exposing (textureCube, fireCube, fogMountainsCube, voronoiCube)
import Things.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Things.Sphere exposing (cloudsSphere)
import Things.Terrain exposing (Terrain)
import Things.Ground exposing (renderGround)
import Things.Surface2D exposing (Placement, defaultPlacement)
import Things.Terrain as Terrain

import Things.BFly exposing (bfly)
import Shaders.VoronoiDistances exposing (voronoiDistances)

{-| Generate a View from a Model
-}
view : Model -> Html Msg
view model =
    case (model.maybeWindowSize, model.maybeTexture, model.maybeTerrain) of
        (Nothing, _, _) -> text ""
        (_, Nothing, _) -> text ""
        (_, _, Nothing) -> text ""
        (Just windowSize, Just texture, Just terrain) ->
            layoutScene windowSize texture terrain model

layoutScene : Window.Size -> WebGL.Texture -> Terrain -> Model.Model-> Html Msg
layoutScene windowSize texture terrain model =
    if model.person.cameraVR then
        layoutSceneVR windowSize texture terrain model
    else
        layoutScene1 windowSize texture terrain model

layoutScene1 : Window.Size -> WebGL.Texture -> Terrain -> Model.Model-> Html Msg
layoutScene1 windowSize texture terrain model =
    div
        [ style
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            , ( "position", "relative" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ WebGL.toHtml
            [ width windowSize.width
            , height windowSize.height
            , style [ ( "display", "block" ) ]
            ]
            (renderWorld Model.OneEye windowSize texture terrain model model.person)
        , div
            [ style
                [ ( "position", "absolute" )
                , ( "font-family", "monospace" )
                , ( "text-align", "center" )
                , ( "left", "20px" )
                , ( "right", "20px" )
                , ( "top", "20px" )
                ]
            ]
            (if model.isLocked then
                exitMsg
             else
                enterMsg
            )
        ]

layoutSceneVR : Window.Size -> WebGL.Texture -> Terrain -> Model.Model-> Html Msg
layoutSceneVR windowSize texture terrain model =
    div
        [ style
            [ ( "width", toString windowSize.width ++ "px" )
            , ( "height", toString windowSize.height ++ "px" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ WebGL.toHtml
            [ width (windowSize.width//2)
            , height windowSize.height
            , style [ ( "display", "block" )
                    , ( "float", "left" )
                    ]
            ]
            (renderWorld Model.LeftEye windowSize texture terrain model model.person)
        , WebGL.toHtml
            [ width (windowSize.width//2)
            , height windowSize.height
            , style [ ( "display", "block" )
                    , ( "float", "right" )
                    ]
            ]
            (renderWorld Model.RightEye windowSize texture terrain model model.person)
        ]

translateP position p = { p | viewMatrix = M4.translate position p.viewMatrix }

mapApply : List (a -> List b) -> a -> List b
mapApply fs x = List.concat <| List.map (\f -> f x) fs

place : Float -> Float -> Float -> Thing -> Thing
place x y z (Thing _ o s) = Thing (vec3 x y z) o s

orient : Thing -> See
orient (Thing position orientation see) =
    let z_axis = vec3 0 0 1
        rot_angle = 0 - acos (dot orientation z_axis)
        rot_axis = normalize (cross orientation z_axis)
    in
        tview (M4.translate position) << tview (M4.rotate rot_angle rot_axis) <| see

eyeOffset : Model.Person -> Model.Eye -> Vec3
eyeOffset person eye =
    if eye == Model.LeftEye then
        Orientation.rotateLabV person.orientation (vec3 (-0.04) 0 0)
    else if eye == Model.RightEye then
        Orientation.rotateLabV person.orientation (vec3 0.04 0 0)
    else
        vec3 0 0 0

aboveTerrain : Model.EyeLevel -> Vec3 -> Vec3
aboveTerrain eyeLevel pos =
    let
        p = toRecord pos
        e = eyeLevel pos
    in
        if p.y < e then vec3 p.x e p.z else pos

{-| Set up 3D world
-}
renderWorld : Model.Eye -> Window.Size -> WebGL.Texture -> Terrain -> Model.Model -> Model.Person -> List WebGL.Renderable
renderWorld eye windowSize texture terrain model person =
    let
        -- placement = defaultPlacement
        eyeLevel pos = Model.eyeLevel + Terrain.elevation terrain pos
        lensDistort = if person.cameraVR then 0.85 else 0.9

        p = { cameraPos = Terrain.bounds terrain (aboveTerrain eyeLevel person.pos)
            , viewMatrix = perspective windowSize person eye
            , globalTime = model.lifetime
            , windowSize = windowSize
            , lensDistort = lensDistort
            , measuredFPS = 30.0
            }

        boidThings = List.map extractThing model.boids
        ballThings = List.map extractThing model.balls
        things = terrain.groundMesh ++ terrain.waterMesh ++ boidThings ++ ballThings
        seeThings = mapApply (List.map orient things)

        worldObjects = List.concat
            [ fogMountainsDiamond (translateP (vec3 0 1.5 0) p)
            , cloudsDiamond (translateP (vec3 5 1.5 1) p)
            , cloudsSphere (translateP (vec3 3 10 5) p)
            , voronoiCube (translateP (vec3 10 0 10) p)
            , fireCube (translateP (vec3 -10 0 -10) p)
            , fogMountainsCube (translateP (vec3 10 1.5 -10) p)
            , textureCube texture (translateP (vec3 -2 0 -17) p)
            -- , renderGround p
            ]
    in
        seeThings p ++ worldObjects

{-| Calculate the viewer's field of view
-}
perspective : Window.Size -> Model.Person -> Model.Eye -> Mat4
perspective { width, height } person eye =
    M4.mul (M4.makePerspective 45 (toFloat width / toFloat height) 0.01 100)
        (M4.makeLookAt (person.cameraPos `add` eyeOffset person eye)
                       (person.pos `add` (scale 3 (Model.direction person)))
                       person.cameraUp)

enterMsg : List (Html Msg)
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : List (Html Msg)
exitMsg = message "Press <escape> to exit full screen."

message : String -> List (Html Msg)
message msg =
    [ p [] [ Html.text "WASD keys to move, space bar to jump." ]
    , p [] [ Html.text msg ]
    ]
