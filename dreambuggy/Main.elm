module Main exposing (main)

import Math.Vector3 exposing (vec3)
import Model exposing (Args)
import NewWorld
import Boids
import Balls
import Sky
import Statue
import TextureCube
import StaticGround
import Wedge
import Body.Terrain as Terrain
import Body.Cube exposing (skyCube, fireCube, fogMountainsCube, voronoiCube)
import Body.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Body.Sphere exposing (skySphere, cloudsSphere)


main : Program Args (Model.Model NewWorld.Model) (Model.Msg NewWorld.Msg)
main =
    NewWorld.create
        { apps =
            [ TextureCube.create "Wooden crate" "resources/woodCrate.jpg"
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
