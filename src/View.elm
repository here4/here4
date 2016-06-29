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
import Thing exposing (..)
-- import Model
import Things.Cube exposing (textureCube, fireCube, fogMountainsCube, voronoiCube)
import Things.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Things.Sphere exposing (cloudsSphere)
-- import Things.Ground exposing (renderGround)
import Things.Surface2D exposing (Placement, defaultPlacement)
import Things.Terrain as Terrain


{-| Generate a View from a Model
-}
view : Model -> Html Msg
view { person, lifetime, maybeWindowSize, maybeTexture, maybeTerrain, isLocked } =
    case (maybeWindowSize, maybeTexture, maybeTerrain) of
        (Nothing, _, _) -> text ""
        (_, Nothing, _) -> text ""
        (_, _, Nothing) -> text ""
        (Just windowSize, Just texture, Just terrain) ->
            layoutScene windowSize lifetime isLocked texture terrain person

layoutScene : Window.Size -> Time -> Bool -> WebGL.Texture -> Array2D Float -> Model.Person -> Html Msg
layoutScene windowSize t isLocked texture terrain person =
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
            (renderWorld windowSize t texture terrain person)
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
            (if isLocked then
                exitMsg
             else
                enterMsg
            )
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

{-| Set up 3D world
-}
renderWorld : Window.Size -> Time -> WebGL.Texture -> Array2D Float -> Model.Person -> List WebGL.Renderable
renderWorld windowSize t texture terrain person =
    let
        p = { cameraPos = person.position
            , viewMatrix = perspective windowSize person
            , globalTime = t
            , windowSize = windowSize
            , lensDistort = 0.9
            , measuredFPS = 7.0
            }

        placement = defaultPlacement

        -- seed0 = Random.initialSeed 7777
        -- (terrain, seed1) = Random.generate (randTerrain2D (placement.bigSide+1)) seed0

        ground = Terrain.paint Terrain.mountains defaultPlacement terrain
        water = Terrain.ripplePaint Terrain.sea 0.3 defaultPlacement terrain

        things = ground ++ water

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
perspective : Window.Size -> Model.Person -> Mat4
perspective { width, height } person =
    M4.mul (M4.makePerspective 45 (toFloat width / toFloat height) 0.01 100)
        (M4.makeLookAt person.position (person.position `add` Model.direction person) j)

enterMsg : List (Html Msg)
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : List (Html Msg)
exitMsg = message "Press <escape> to exit full screen."

message : String -> List (Html Msg)
message msg =
    [ p [] [ Html.text "WASD keys to move, space bar to jump." ]
    , p [] [ Html.text msg ]
    ]
