module Behavior.Boids exposing (..)

import Here4.Body exposing (..)
import Here4.Ground exposing (Ground)
import Here4.Orientation exposing (..)
import Math.Vector3 as V3
import Math.Vector3 exposing (Vec3, vec3)
import Time exposing (Time)


type alias Attributes =
    { separation : Float
    , alignment : Float
    , cohesion : Float
    }


default : Attributes
default =
    { separation = 0.5
    , alignment = 0.03 -- 0.07
    , cohesion = 0.01
    }


type alias Boid a =
    Massive (Spherical (Moving a))


newBoid : Float -> Float -> Vec3 -> Vec3 -> Visible (Oriented {}) -> Boid (Visible {})
newBoid m r pos vel thing0 =
    { radius = r
    , mass = m
    , velocity = vel
    , scale = thing0.scale
    , position = pos
    , orientation = thing0.orientation
    , appear = thing0.appear
    }


boidOrientation : Moving a -> Orientation
boidOrientation b =
    fromTo V3.k b.velocity


stepBoid : Ground -> Time -> Spherical (Moving a) -> Spherical (Moving a)
stepBoid ground dt b =
    { b
        | position = ground.bounds b.radius (V3.add b.position ((V3.scale dt b.velocity)))
        , orientation = boidOrientation b
    }


rule1 : Float -> Int -> Vec3 -> Boid a -> Vec3
rule1 cohesion n sumPos b =
    let
        perceived_scale =
            1.0 / (toFloat (n - 1))

        perceived_center =
            V3.scale perceived_scale (V3.sub sumPos b.position)
    in
        V3.scale cohesion <| V3.sub perceived_center b.position


rule2 : Float -> List Vec3 -> Boid a -> Vec3
rule2 separation poss b =
    let
        f pos =
            let
                d =
                    V3.distanceSquared pos b.position
            in
                if (d > 0 && d < 10.0) then
                    V3.sub b.position pos
                else
                    vec3 0 0 0
    in
        V3.scale separation <| sumVec3s (List.map f poss)


rule3 : Float -> Int -> Vec3 -> Boid a -> Vec3
rule3 alignment n sumVel b =
    let
        perceived_scale =
            1.0 / (toFloat (n - 1))

        perceived_vel =
            V3.scale perceived_scale (V3.sub sumVel b.velocity)
    in
        V3.scale alignment <| V3.sub perceived_vel b.velocity


bounds : Ground -> Boid a -> Vec3
bounds ground b =
    let
        bound x low high =
            if (x < low) then
                1
            else
                (if x > high then
                    -1
                 else
                    0
                )

        ( x, y, z ) =
            V3.toTuple b.position

        elevation =
            max 0 (ground.elevation b.position)
    in
        vec3 (bound x -200 200)
            (bound y (elevation + 10 + b.radius) (elevation + 100))
            (bound z -200 200)


boundVelocity : Vec3 -> Vec3
boundVelocity v =
    let
        l =
            V3.length v
    in
        if (l < 0) then
            vec3 0 0 0
        else if (l < 1) then
            (V3.scale (1 / l) v)
        else
            v


moveBoids : Attributes -> Ground -> Time -> List (Boid a) -> List (Boid a)
moveBoids attributes ground dt boids =
    let
        nboids =
            List.length boids

        positions =
            List.map .position boids

        velocities =
            List.map .velocity boids

        sumPos =
            sumVec3s positions

        sumVel =
            sumVec3s velocities

        r1s =
            List.map (rule1 attributes.cohesion nboids sumPos) boids

        r2s =
            List.map (rule2 attributes.separation positions) boids

        r3s =
            List.map (rule3 attributes.alignment nboids sumVel) boids

        box =
            List.map (bounds ground) boids

        applyRules b r1 r2 r3 r4 =
            { b
                | velocity =
                    boundVelocity
                        (V3.add b.velocity
                            (V3.scale dt
                                (V3.add r1 (V3.add r2 (V3.add r3 r4)))
                            )
                        )
            }

        bs =
            List.map5 applyRules boids r1s r2s r3s box
    in
        List.map (stepBoid ground dt) bs


sumVec3s : List Vec3 -> Vec3
sumVec3s =
    List.foldl V3.add (vec3 0 0 0)
