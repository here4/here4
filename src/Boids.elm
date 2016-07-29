module Boids exposing (Boids, init, update, animate, things)

import Math.Vector3 exposing (add, vec3)
import Random
import Time exposing (Time)

import Behavior.Boids exposing (..)
import Math.RandomVector exposing (randomVec3')
import Thing exposing (..)
import Things.BFly exposing (bfly)

import Shaders.VoronoiDistances exposing (voronoiDistances)

type alias Boids = List (Boid (Visible {}))

randomBFly : Random.Generator (Visible (Oriented {}))
randomBFly = Random.map (bfly voronoiDistances) (Random.float 0.0 1.0)

randomBoid : Random.Generator (Boid (Visible {}))
randomBoid = Random.map3
    (newBoid 0.3 1.0)
    (Random.map (add (vec3 7 8 4)) (randomVec3' 4.0))
    (randomVec3' 1.0)
    randomBFly

randomBoids : Int -> Random.Generator Boids
randomBoids n = Random.list n randomBoid

init : Int -> (Boids, Cmd ThingMsg)
init n = ([], Random.generate (wrapMsg "boidsgenerated") (randomBoids n))

update : ThingMsg -> Boids -> (Boids, Cmd ThingMsg)
update msg boids = let (_, newBoids) = unwrapMsg msg in (newBoids, Cmd.none)

animate : Time -> Boids -> Boids
animate dt boids = moveBoids dt boids

things : Boids -> List Thing
things = List.map extractThing
