module View exposing (view)

import Color exposing (black, white)
import FontAwesome
-- import Html
import Html exposing (Html, text, div, p, span)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as M4
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as V3
import Math.Vector3 exposing (..)
import Random
import Time exposing (Time)
import WebGL
import Window

import Array2D exposing (Array2D)
import Model exposing (Model, Msg)
import Orientation
import Thing exposing (..)
import Things.Cube exposing (textureCube, fireCube, fogMountainsCube, voronoiCube)
import Things.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Things.Sphere exposing (cloudsSphere)
import Things.Terrain exposing (Terrain)
import Things.Ground exposing (renderGround)
import Things.Surface2D exposing (Placement, defaultPlacement)
import Things.Terrain as Terrain

import Shaders.VoronoiDistances exposing (voronoiDistances)

{-| Generate a View from a Model
-}
view : (worldModel -> Maybe Model.World)
     -> Model worldModel -> Html (Msg worldMsg)
view worldView model =
    case (model.maybeWindowSize, worldView model.worldModel) of
        (Nothing, _) -> text ""
        (_, Nothing) -> text ""
        (Just windowSize, Just world) ->
            layoutScene windowSize model world

layoutScene : Window.Size -> Model worldModel -> Model.World -> Html (Msg worldMsg)
layoutScene windowSize model world =
    let
        render = renderWorld model.globalTime world
    in
        if model.person.cameraVR then
            layoutSceneVR windowSize model render
        else if model.numPlayers == 2 then
            layoutScene2 windowSize model render
        else
            layoutScene1 windowSize model render

type alias RenderWorld = Model.Eye -> Window.Size -> Model.Person -> List WebGL.Renderable

layoutScene1 : Window.Size -> Model worldModel -> RenderWorld -> Html (Msg worldMsg)
layoutScene1 windowSize model render =
    div
        [ style
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ WebGL.toHtml
            [ width windowSize.width
            , height windowSize.height
            , style [ ( "display", "block" )
                    , ( "position", "absolute" )
                    , ( "top", "0px" )
                    , ( "left", "0px" )
                    , ( "right", "0px" )
                    ]
            ]
            (render Model.OneEye windowSize model.person)
        , hud model.person 0 0
        ]

layoutScene2 : Window.Size -> Model worldModel -> RenderWorld -> Html (Msg worldMsg)
layoutScene2 windowSize model render =
    let w2 = windowSize.width // 2
        ws2 = { windowSize | width = w2 }
    in
      div
        [ style
            [ ( "width", toString windowSize.width ++ "px" )
            , ( "height", toString windowSize.height ++ "px" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ span [] [
            div []
            [ WebGL.toHtml
              [ width w2
              , height windowSize.height
              , style [ ( "display", "block" )
                      , ( "float", "left" )
                      , ( "position", "absolute" )
                      , ( "top", "0px" )
                      , ( "left", "0px" )
                      , ( "right", toString w2 ++ "px" )
                      , ( "border", "0px" )
                      , ( "padding", "0px" )
                      ]
              ]
              (render Model.OneEye ws2 model.person)
            , hud model.person 0 w2
            ]
          , div []
            [ WebGL.toHtml
              [ width w2
              , height windowSize.height
              , style [ ( "display", "block" )
                      , ( "float", "right" )
                      , ( "position", "absolute" )
                      , ( "top", "0px" )
                      , ( "left", toString w2 ++ "px" )
                      , ( "right", "0px" )
                      , ( "border", "0px" )
                      , ( "padding", "0px" )
                      ]
              ]
              (render Model.OneEye ws2 model.player2)
            , hud model.player2 w2 0
            ]
          ]
        ]

layoutSceneVR : Window.Size -> Model worldModel -> RenderWorld -> Html (Msg worldMsg)
layoutSceneVR windowSize model render =
    let w2 = windowSize.width // 2
        ws2 = { windowSize | width = w2 }
    in
      div
        [ style
            [ ( "width", toString windowSize.width ++ "px" )
            , ( "height", toString windowSize.height ++ "px" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ WebGL.toHtml
            [ width w2
            , height windowSize.height
            , style [ ( "display", "block" )
                    , ( "float", "left" )
                    ]
            ]
            (render Model.LeftEye ws2 model.person)
        , WebGL.toHtml
            [ width w2
            , height windowSize.height
            , style [ ( "display", "block" )
                    , ( "float", "right" )
                    ]
            ]
            (render Model.RightEye ws2 model.person)
        ]

mapApply : List (a -> List b) -> a -> List b
mapApply fs x = List.concat <| List.map (\f -> f x) fs

orient : Thing -> See
orient (Thing scale position orientation see) =
    let z_axis = vec3 0 0 1
        rot_angle = 0 - acos (dot orientation z_axis)
        rot_axis = normalize (cross orientation z_axis)
    in
        tview (M4.scale scale) << tview (M4.translate position) << tview (M4.rotate rot_angle rot_axis) <| see

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
renderWorld : Time -> Model.World -> Model.Eye -> Window.Size -> Model.Person -> List WebGL.Renderable
renderWorld globalTime world eye windowSize person =
    let
        eyeLevel pos = Model.eyeLevel + Terrain.elevation world.terrain pos
        lensDistort = if person.cameraVR then 0.85 else 0.95

        p = { cameraPos = Terrain.bounds world.terrain (aboveTerrain eyeLevel person.pos)
            , viewMatrix = perspective windowSize person eye
            , globalTime = globalTime
            , windowSize = windowSize
            , lensDistort = lensDistort
            , cameraVR = person.cameraVR
            , measuredFPS = 30.0
            }

        skybox = orientSkybox world.skybox { p | viewMatrix = skyboxMatrix windowSize person }

        things = world.terrain.groundMesh ++ world.terrain.waterMesh ++ world.things
        seeThings = mapApply (List.map orient things)
    in
        skybox ++ seeThings p

{-| Calculate the viewer's field of view
-}
perspective : Window.Size -> Model.Person -> Model.Eye -> Mat4
perspective { width, height } person eye =
    M4.mul (M4.makePerspective 45 (toFloat width / toFloat height) 0.01 100)
        (M4.makeLookAt (person.cameraPos `add` eyeOffset person eye)
                       (person.pos `add` (scale 3 (Model.direction person)))
                       person.cameraUp)

orientSkybox : Thing -> See
orientSkybox (Thing scale _ orientation see) =
    let z_axis = vec3 0 0 1
        rot_angle = 0 - acos (dot orientation z_axis)
        rot_axis = normalize (cross orientation z_axis)
    in
        tview (M4.scale scale) << tview (M4.rotate rot_angle rot_axis) <| see

skyboxMatrix : Window.Size -> Model.Person -> Mat4
skyboxMatrix { width, height } person =
    M4.mul (M4.makePerspective 45 (toFloat width / toFloat height) 0.01 100)
        (M4.makeLookAt (vec3 0 0 0)
                       (scale 3 (Model.direction person))
                       person.cameraUp)

hud : Model.Person -> Int -> Int -> Html (Msg worldMsg)
hud person left right =
    let
        vehicleName = if person.vehicle == Model.vehicleBird then
                          "Dreambird"
                      else if person.vehicle == Model.vehicleBuggy then
                           "Dreambuggy"
                      else if person.vehicle == Model.vehicleLookAt then
                           "Look at"
                      else "DreamDebug"
        wher = if person.cameraInside then "Inside" else "Outside"
    in div
       [ style
           [ ( "position", "absolute" )
           , ( "font-family", "Verdana, Geneva, sans-serif" )
           , ( "text-align", "center" )
           , ( "left", toString left ++ "px" )
           , ( "right", toString right ++ "px" )
           , ( "top", "0px" )
           , ( "border", "0px" )
           , ( "padding", "0px" )
           , ( "background-color", "rgba(0,0,0,0.5)" )
           , ( "color", "#fff" )
           , ( "font-size", "xx-large" )
           , ( "text-shadow", "1px 0 0 #000, 0 -1px 0 #000, 0 1px 0 #000, -1px 0 0 #000" )
           , ( "z-index", "1" )
           ]
       ]
       [
           span []
           [ Html.text vehicleName
           , Html.text " "
           , FontAwesome.diamond white 20
           , Html.text " "
           , Html.text wher
           ]
       ]

enterMsg : List (Html Msg)
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : List (Html Msg)
exitMsg = message "Press <escape> to exit full screen."

message : String -> List (Html Msg)
message msg =
    [ p [] [ Html.text "Use gamepad, arrows or WASD keys to move." ]
    , p [] [ Html.text msg ]
    ]
