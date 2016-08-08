module Balls exposing (create)

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

type Msg = BallsGenerated Balls

create : Int -> (Things, Cmd ThingMsg)
create n = createThings (init n)
    { update = update
    , animate = animate
    , things = things
    , focus = focus
    }

randomDrop : Random.Generator (Drop (Visible {}))
randomDrop = Random.map2
    (\pos vel -> newDrop pos vel fogMountainsSphere)
    (Random.map (add (vec3 0 30 0)) (randomVec3' 4.0))
    (randomVec3' 8.0)

randomBalls : Int -> Random.Generator Balls
randomBalls n = Random.list n randomDrop

init : Int -> (Balls, Cmd (MyMsg Msg))
init n = ([], Random.generate (My << BallsGenerated) (randomBalls n))

update : MyMsg Msg -> Balls -> (Balls, Cmd (MyMsg Msg))
update msg balls = case msg of
    My (BallsGenerated newBalls) -> (newBalls, Cmd.none)
    _ -> (balls, Cmd.none)

animate : Time -> Balls -> Balls
animate dt balls = collisions dt (moveDrops dt balls)

things : Balls -> List Thing
things = List.map extractThing

focus : Balls -> Maybe Focus
focus balls = Maybe.map orientedToFocus (List.head balls)
