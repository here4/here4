module Physics.Gravity exposing (gravity)

import Math.Vector3 as V3
import Math.Vector3 exposing (Vec3, vec3)
import Time exposing (Time)

import Body exposing (..)
import Ground exposing (Ground)
import Orientation exposing (Orientation, fromVec3)

type alias BBall a = Massive (Spherical (Moving a))

-- TODO: add spin?
dropOrientation : Moving a -> Orientation
dropOrientation d = fromVec3 d.velocity

bounds : Ground -> BBall a -> BBall a
bounds ground b =
    let bound vs s low high = let dampVs = -vs * 0.99 in
            if vs < 0 && s < low then
                dampVs
            else if vs > 0 && s > high then
                dampVs
            else
                vs
        (x,y,z) = V3.toTuple b.position
        (vx,vy,vz) = V3.toTuple b.velocity
    in { b | velocity = vec3 (bound vx x -100 100) (bound vy y (b.radius) 100) (bound vz z -50 50) }

gForce : a -> Vec3
gForce _ = vec3 0 -9.8 0

gravity : Ground -> Time -> List (BBall a) -> List (BBall a)
gravity ground dt balls =
    let
        gs = List.map gForce balls
        applyRules b g = { b |
            velocity = (V3.add b.velocity (V3.scale dt g)) }
        bs = List.map (bounds ground) <| List.map2 applyRules balls gs
    in List.map (\x -> { x | orientation = dropOrientation x }) <| bs
