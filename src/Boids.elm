module Boids exposing (create)

import Math.Vector3 exposing (add, vec3)
import Random
import Time exposing (Time)

import Behavior.Boids exposing (..)
import Math.RandomVector exposing (randomVec3)
import Body exposing (..)
import App exposing (..)
import Things.BFly exposing (bfly)

import Shaders.VoronoiDistances exposing (voronoiDistances)

type alias Boids = List (Boid (Visible {}))

type Msg = BoidsGenerated Boids

create : Int -> (App, Cmd AppMsg)
create n = App.createUncontrolled (init n)
    { update = update
    , animate = animate
    , bodies = bodies
    , focus = focus
    }

randomBFly : Random.Generator (Visible (Oriented {}))
randomBFly = Random.map (bfly voronoiDistances) (Random.float 0.0 1.0)

randomBoid : Random.Generator (Boid (Visible {}))
randomBoid = Random.map3
    (newBoid 0.3 1.0)
    (Random.map (add (vec3 7 8 4)) (randomVec3 4.0))
    (randomVec3 1.0)
    randomBFly

randomBoids : Int -> Random.Generator Boids
randomBoids n = Random.list n randomBoid

init : Int -> (Boids, Cmd Msg)
init n = ([], Random.generate BoidsGenerated (randomBoids n))

update : Msg -> Boids -> (Boids, Cmd Msg)
update msg model = case msg of
   BoidsGenerated newBoids -> (newBoids, Cmd.none)

animate : Time -> Boids -> Boids
animate dt boids = moveBoids dt boids

bodies : Boids -> List Body
bodies = List.map toBody

focus : Boids -> Maybe Focus
focus boids = Maybe.map orientedToFocus (List.head boids)

