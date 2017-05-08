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
import Body.Terrain as Terrain

import Body.Cube exposing (skyCube, fireCube, fogMountainsCube, voronoiCube)
import Body.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Body.Sphere exposing (skySphere, cloudsSphere)

main : Program Args
        (Model.Model NewWorld.Model)
        (Model.Msg NewWorld.Msg)
main =
  NewWorld.create
    { apps =
        [ StaticGround.create Terrain.generate
        , Sky.create skySphere
        , TextureCube.create "resources/woodCrate.jpg"
        , Boids.create 100
        , Balls.create 30
        , Statue.create (vec3 3 10 5) cloudsSphere
        , Statue.create (vec3 0 1.5 0) fogMountainsDiamond
        , Statue.create (vec3 5 1.5 1) cloudsDiamond
        , Statue.create (vec3 10 0 10) voronoiCube
        , Statue.create (vec3 -10 0 -10) skyCube -- fireCube
        , Statue.create (vec3 10 1.5 -10) fogMountainsCube
        ]
    }
