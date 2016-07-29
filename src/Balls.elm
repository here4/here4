module Balls exposing (Balls, init, update, animate, things)

import Math.Vector3 exposing (add, vec3)
import Random
import Time exposing (Time)

import Thing exposing (..)
import Things.Sphere exposing (fogMountainsSphere)

import Math.RandomVector exposing (randomVec3')
import Physics.Collisions exposing (collisions)
import Physics.Drop exposing (..)
import Shaders.FogMountains exposing (fogMountains)

type alias Balls = List (Drop (Visible {}))

randomDrop : Random.Generator (Drop (Visible {}))
randomDrop = Random.map2
    (\pos vel -> newDrop pos vel fogMountainsSphere)
    (Random.map (add (vec3 0 30 0)) (randomVec3' 4.0))
    (randomVec3' 8.0)

randomBalls : Int -> Random.Generator Balls
randomBalls n = Random.list n randomDrop

init : Int -> (Balls, Cmd ThingMsg)
init n = ([], Random.generate (wrapMsg "ballsgenerated") (randomBalls n))

update : ThingMsg -> Balls -> (Balls, Cmd ThingMsg)
update msg balls = let (_, newBalls) = unwrapMsg msg in (newBalls, Cmd.none)

animate : Time -> Balls -> Balls
animate dt balls = collisions dt (moveDrops dt balls)

things : Balls -> List Thing
things = List.map extractThing
