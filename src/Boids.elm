module Boids exposing (randomBoids)

import Math.Vector3 exposing (add, vec3)
import Random

import Thing exposing (..)
import Things.BFly exposing (bfly)

import Math.RandomVector exposing (randomVec3')
import Behavior.Boids exposing (..)
import Shaders.VoronoiDistances exposing (voronoiDistances)

randomBFly : Random.Generator (Visible (Oriented {}))
randomBFly = Random.map (bfly voronoiDistances) (Random.float 0.0 1.0)

randomBoid : Random.Generator (Boid (Visible {}))
randomBoid = Random.map3
    (newBoid 0.3 1.0)
    (Random.map (add (vec3 7 8 4)) (randomVec3' 4.0))
    (randomVec3' 1.0)
    randomBFly

randomBoids : Int -> Random.Generator (List (Boid (Visible {})))
randomBoids n = Random.list n randomBoid
