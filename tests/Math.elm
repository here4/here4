module Math exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, conditional, float)

import Orientation
import Math.Vector3 as V3 exposing (Vec3)

vec3 : Fuzzer Vec3
vec3 =
    Fuzz.map3 V3.vec3 float float float

nonZeroVec3 : Fuzzer Vec3
nonZeroVec3 =
    conditional
        { retries = 1
        , fallback = always V3.i
        , condition = \v -> V3.length v /= 0
        }
        vec3

suite : Test
suite =
    describe "Math tests"
        [ test "Project (1,1,1) onto the X,Y plane" <|
            \() ->
                let proj = Orientation.v3_projectPlane V3.i V3.j (V3.vec3 1 1 1)
                    crossX = V3.cross V3.i proj
                    crossY = V3.cross V3.j proj
                    dotX = V3.dot crossX
                    dotY = V3.dot crossY
                in
                    Expect.all
                        [ (dotX >> Expect.equal 0)
                        , (dotY >> Expect.equal 0)
                        ]
                        proj
{-
        , test "Upright" <|
            \() ->
                let
                    o = Orientation.fromAngleAxis (pi/4) V3.k
                    upr = Orientation.upright o
                in
                    Expect.all
                        [ (Expect.equal Orientation.initial)
                        ]
                        upr
        , fuzz3 nonZeroVec3 nonZeroVec3 nonZeroVec3 "Project vector onto plane" <|
            \v1 v2 u ->
                let proj = Orientation.v3_projectPlane v1 v2 u
                    cross1 = V3.cross v1 proj
                    cross2 = V3.cross v2 proj
                    dot1 = V3.dot cross1
                    dot2 = V3.dot cross2
                in
                    Expect.all
                        [ (dot1 >> Expect.equal 0)
                        , (dot2 >> Expect.equal 0)
                        ]
                        proj
-}
        ] 
