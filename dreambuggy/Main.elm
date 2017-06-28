module Main exposing (main)

import App exposing (App, AppMsg)
import Html exposing (Html)
import Math.Vector3 exposing (vec3)
import Model exposing (Args)
import NewWorld
import Boids
import Balls
import Obj
import Shufflepuck
import Sky
import Statue
import TextureCube
import StaticGround
import Suzanne
import Wedge
import Body.Terrain as Terrain
import Primitive.Cube exposing (skyCube, fireCube, fogMountainsCube, voronoiCube)
import Primitive.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Primitive.Sphere exposing (skySphere, cloudsSphere)
import Vehicles.DreamBuggy as DreamBuggy


main : Program Args (Model.Model NewWorld.Model NewWorld.Msg) (Model.Msg NewWorld.Msg)
main =
    NewWorld.create
        { apps =
            [ elmLogo
            , Suzanne.create "Suzanne"
            , TextureCube.create "Wooden crate" "resources/woodCrate.jpg"
            , let s = Shufflepuck.default in Shufflepuck.create { s | position = vec3 53 0 18 }
            , Wedge.create "Wedge" (vec3 23 0 12)
            , Statue.create 3.0 "Clouds Sphere" (vec3 3 10 5) cloudsSphere
            , Statue.create 2.0 "Landscape Diamond" (vec3 0 1.5 0) fogMountainsDiamond
            , Statue.create 4.0 "Sky Diamond" (vec3 5 1.5 1) cloudsDiamond
            , Statue.create 7.0 "Voronoi Cube" (vec3 10 0 10) voronoiCube
            , Statue.create 3.0 "Fire Cube" (vec3 -10 0 -10) fireCube
            , Statue.create 4.0 "Landscape Cube" (vec3 10 1.5 -10) fogMountainsCube
            , Boids.create 100
            , Balls.create 30
            , StaticGround.create Terrain.generate
            , Sky.create skySphere
            ]
        }

elmLogo : (App, Cmd AppMsg)
elmLogo =
    let
        overlay =
            Html.div []
                [ Html.h2 []
                      [ Html.text "Elm Logo" ]
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]
        drive = DreamBuggy.drive { speed = 8.0 }
    in
        Obj.create
            { label = "Elm Logo"
            , overlay = overlay
            , meshPath = "meshes/elmLogo.obj"
            , diffuseTexturePath = "textures/elmLogoDiffuse.png"
            , normalTexturePath = "textures/elmLogoNorm.png"
            , drive = Just drive
            }
        
