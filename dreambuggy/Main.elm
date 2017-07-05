module Main exposing (main)

import App exposing (App, AppMsg)
import Html exposing (Html)
import Math.Vector3 exposing (vec3)
import Model exposing (Args)
import MultiWorld
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


main : Program Args (Model.Model MultiWorld.Model MultiWorld.Msg) (Model.Msg MultiWorld.Msg)
main =
    MultiWorld.create
        { apps =
            [ elmLogo
            , TextureCube.create "Wooden crate" "resources/woodCrate.jpg"
            , let s = Shufflepuck.default in Shufflepuck.create { s | position = vec3 53 0 18 }
            , Wedge.create "Wedge" (vec3 23 0 12)
            , Statue.create { label = "Clouds Sphere", position = vec3 3 10 5, appear = cloudsSphere }
            , Statue.create { label = "Landscape Diamond", position = vec3 0 1.5 0, appear = fogMountainsDiamond }
            , Statue.portal (vec3 -30 14 31) { label = "Sky Diamond", position = vec3 5 1.5 1, appear = cloudsDiamond }
            , Statue.create { label = "Landscape Cube", position = vec3 10 1.5 -10, appear = fogMountainsCube }

            , Statue.portal ( vec3 120 17 -261 )
                { label = "Voronoi Cube", position = vec3 10 0 10, appear = voronoiCube }
            , Statue.create { label = "Fire Cube", position = vec3 121 0 -253, appear = fireCube }

            , Boids.create 100
            , Balls.create 30
            , StaticGround.create Terrain.generate
            , Sky.create skySphere
            ]
        , defaultSelf = Suzanne.create { label = "Walking", height = 1.4, speed = 2.0 }
        -- , defaultSelf = Statue.create "Hippy actualization" (vec3 30 50 6) cloudsSphere
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
        
