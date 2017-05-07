module Main exposing (main)

import Math.Vector3 exposing (vec3)

import Model exposing (Args)

import Body exposing (resize, put)
import Control exposing (CtrlMsg)

import TerrainWorld
import Boids
import Balls
import Statue
import TextureCube

import Body.Cube exposing (skyCube, fireCube, fogMountainsCube, voronoiCube)
import Body.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Body.Sphere exposing (skySphere, cloudsSphere)

main : Program Args
        (Model.Model TerrainWorld.WorldModel)
        (Model.Msg TerrainWorld.WorldMsg)
main =
  TerrainWorld.create
    { apps =
        [ TextureCube.create "resources/woodCrate.jpg"
        , Boids.create 100
        , Balls.create 30
        , Statue.create (vec3 3 10 5) cloudsSphere
        , Statue.create (vec3 0 1.5 0) fogMountainsDiamond
        , Statue.create (vec3 5 1.5 1) cloudsDiamond
        , Statue.create (vec3 10 0 10) voronoiCube
        , Statue.create (vec3 -10 0 -10) skyCube -- fireCube
        , Statue.create (vec3 10 1.5 -10) fogMountainsCube
        ]
    , skybox = resize 80 <| put (vec3 0 1 1) skySphere
    }
