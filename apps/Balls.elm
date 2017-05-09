module Balls exposing (create)

import Math.Vector3 exposing (Vec3, add, vec3)
import Random
import Time exposing (Time)

import App exposing (..)
import Body exposing (..)
import Math.RandomVector exposing (randomVec3)
import Physics.Collisions exposing (collisions)
import Physics.Gravity exposing (gravity)

import Body.Sphere exposing (fogMountainsSphere)

type alias BBall a = Massive (Spherical (Moving a))
type alias Balls = List (BBall (Visible {}))

type Msg = BallsGenerated Balls

create : Int -> (App, Cmd AppMsg)
create n = App.createUncontrolled (init n)
    { label = always "Balls"
    , update = update
    , animate = animate
    , bodies = bodies
    , camera = camera
    , focus = focus
    }

newDrop : Vec3 -> Vec3 -> Oriented (Visible {}) -> BBall (Visible {})
newDrop pos vel thing0 =
    { radius = 1.0
    , mass = 1.0
    , velocity = vel
    , scale = thing0.scale
    , pos = pos
    , orientation = thing0.orientation
    , appear = thing0.appear
    }

randomDrop : Random.Generator (BBall (Visible {}))
randomDrop = Random.map2
    (\pos vel -> newDrop pos vel fogMountainsSphere)
    (Random.map (add (vec3 0 30 0)) (randomVec3 4.0))
    (randomVec3 8.0)

randomBalls : Int -> Random.Generator Balls
randomBalls n = Random.list n randomDrop

init : Int -> (Balls, Cmd Msg)
init n = ([], Random.generate BallsGenerated (randomBalls n))

update : Msg -> Balls -> (Balls, Cmd Msg)
update msg balls = case msg of
    BallsGenerated newBalls -> (newBalls, Cmd.none)

animate : Time -> Balls -> Balls
animate dt balls = collisions dt (gravity dt balls)

bodies : Balls -> List Body
bodies = List.map toBody

camera : Balls -> Maybe Camera
camera balls = Maybe.map (bodyCamera << toBody) (List.head balls)

focus : Balls -> Maybe Focus
focus balls = Nothing
