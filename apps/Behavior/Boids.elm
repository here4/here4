module Behavior.Boids exposing (..)

import Quaternion as Qn
import Math.Vector3 as V3
import Math.Vector3 exposing (Vec3, vec3)
import Time exposing (Time)

import Body exposing (..)

type alias Boid a = Massive (Spherical (Moving a))

newBoid : Float -> Float -> Vec3 -> Vec3 -> Visible (Oriented {}) -> Boid (Visible {})
newBoid m r pos vel thing0 =
    { radius = r
    , mass = m
    , velocity = vel
    , scale = thing0.scale
    , pos = pos
    , orientation = thing0.orientation
    , appear = thing0.appear
    }

boidOrientation : Moving a -> Qn.Quaternion
boidOrientation b = Qn.fromVec3 b.velocity
{-
    let v = V3.toRecord b.velocity
    in V3.normalize (vec3 v.x (v.y/10) v.z)
-}

stepBoid : Time -> Moving a -> Moving a
stepBoid dt b = { b | pos = V3.add b.pos ((V3.scale dt b.velocity)), orientation = boidOrientation b }

rule1 : Int -> Vec3 -> Boid a -> Vec3 
rule1 n sumPos b =
    let perceived_scale = 1.0 / (toFloat (n-1))
        perceived_center = V3.scale perceived_scale (V3.sub sumPos b.pos)
    in V3.scale (1/100) <| V3.sub perceived_center b.pos

rule2 : List Vec3 -> Boid a -> Vec3
rule2 poss b =
    let f pos = let d = V3.distanceSquared pos b.pos
                in if (d > 0 && d < 10.0) then V3.sub b.pos pos else vec3 0 0 0
    in V3.scale (1/2) <| sumVec3s (List.map f poss)

rule3 : Int -> Vec3 -> Boid a -> Vec3
rule3 n sumVel b =
    let perceived_scale = 1.0 / (toFloat (n-1))
        perceived_vel = V3.scale perceived_scale (V3.sub sumVel b.velocity)
    in V3.scale (1/15) <| V3.sub perceived_vel b.velocity

bounds : Boid a -> Vec3
bounds b =
    let bound x low high = if (x < low) then 1 else (if x > high then -1 else 0)
        (x,y,z) = V3.toTuple b.pos
    in vec3 (bound x -200 200) (bound y 0 100) (bound z -200 200)

boundVelocity : Vec3 -> Vec3
boundVelocity v = let l = V3.length v in if (l<1) then (V3.scale (1/l) v) else v

moveBoids : Time -> List (Boid a) -> List (Boid a)
moveBoids dt boids =
    let
        nboids = List.length boids
        positions = List.map .pos boids
        velocities = List.map .velocity boids
        sumPos = sumVec3s positions
        sumVel = sumVec3s velocities
        r1s = List.map (rule1 nboids sumPos) boids
        r2s = List.map (rule2 positions) boids
        r3s = List.map (rule3 nboids sumVel) boids
        box = List.map bounds boids
        applyRules b r1 r2 r3 r4 = { b |
            velocity = boundVelocity (V3.add b.velocity (V3.scale dt
                (V3.add r1 (V3.add r2 (V3.add r3 r4))))) }
        bs = List.map5 applyRules boids r1s r2s r3s box
    in List.map (stepBoid dt) bs

{-
boidsTCont : TCont (List (Boid a))
boidsTCont = simpleTCont moveBoids
-}

{-
-- runBoids : Signal [a] -> Signal Time -> Signal (TCont [Boid a])
-- runBoids : Signal (List (Boid a)) -> Signal Time -> Signal (List (Boid a))
runBoids : List (Boid a) -> Signal Time -> Signal (List (Boid a))
-- runBoids boids0 t = foldSigTCont2 [] boidsTCont boids0 t
runBoids = Signal.foldp moveBoids
-}

{-
runBoids : Signal [Boid a] -> Signal Time -> Signal [Boid a]
runBoids 
-}

sumVec3s : List Vec3 -> Vec3
sumVec3s = List.foldl V3.add (vec3 0 0 0)