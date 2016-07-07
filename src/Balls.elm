module Balls exposing (randomBalls)

import Math.Vector3 exposing (add, vec3)
import Random

import Thing exposing (..)
import Things.Sphere exposing (fogMountainsSphere)

import Math.RandomVector exposing (randomVec3')
import Physics.Drop exposing (..)
import Shaders.FogMountains exposing (fogMountains)

randomDrop : Random.Generator (Drop (Visible {}))
randomDrop = Random.map2
    (\pos vel -> newDrop pos vel fogMountainsSphere)
    (Random.map (add (vec3 0 30 0)) (randomVec3' 4.0))
    (randomVec3' 8.0)

randomBalls : Int -> Random.Generator (List (Drop (Visible {})))
randomBalls n = Random.list n randomDrop
