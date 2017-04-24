module Boids exposing (create)

import Math.Vector3 exposing (add, vec3)
import Random
import Time exposing (Time)

import Behavior.Boids exposing (..)
import Dispatch exposing (..)
import Math.RandomVector exposing (randomVec3)
import Thing exposing (..)
import Things.BFly exposing (bfly)

import Shaders.VoronoiDistances exposing (voronoiDistances)

type alias Boids = List (Boid (Visible {}))

type Msg = BoidsGenerated Boids

create : Int -> (Things, Cmd ThingMsg)
create n = createThings (init n)
    { update = update
    , animate = animate
    , things = things
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

init : Int -> (Boids, Cmd (Dispatch CtrlMsg Msg))
init n = ([], Random.generate (Self << BoidsGenerated) (randomBoids n))

update : Dispatch CtrlMsg Msg -> Boids -> (Boids, Cmd (Dispatch CtrlMsg Msg))
update msg model = case msg of
    Self (BoidsGenerated newBoids) -> (newBoids, Cmd.none)
    _ -> (model, Cmd.none)

animate : Time -> Boids -> Boids
animate dt boids = moveBoids dt boids

things : Boids -> List Thing
things = List.map extractThing

focus : Boids -> Maybe Focus
focus boids = Maybe.map orientedToFocus (List.head boids)

