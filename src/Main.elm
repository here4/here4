module Main exposing (main)

import Math.Vector3 exposing (vec3)

import Model exposing (Args)
import TerrainWorld
import Thing exposing (..)

import Boids
import Balls
import TextureCube

import Things.Cube exposing (skyCube, textureCube, cloudsCube, fireCube, fogMountainsCube, voronoiCube)
import Things.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Things.Sphere exposing (skySphere, cloudsSphere)

main : Program Args
main =
  TerrainWorld.create
    { things =
        [ TextureCube.create "resources/woodCrate.jpg"
        , Boids.create 100
        , Balls.create 30
        ]
    , staticThings =
        [ put (vec3 0 1.5 0) fogMountainsDiamond
        , put (vec3 5 1.5 1) cloudsDiamond
        , put (vec3 3 10 5) cloudsSphere
        , put (vec3 10 0 10) voronoiCube
        , put (vec3 -10 0 -10) skyCube -- fireCube
        , put (vec3 10 1.5 -10) fogMountainsCube
        ]
    , skybox = resize 80 <| put (vec3 0 1 1) skySphere
    }
