module Math exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, conditional, float)
import Geometry.Projection as Geometry
import Math.Vector3 as V3 exposing (Vec3)
import MathExpect exposing (..)
import MathFuzz exposing (..)
import Here4.Orientation as Orientation


suite : Test
suite =
    describe "Math tests"
        [ projectPlane
        -- , orientUpright
        ]


projectPlane : Test
projectPlane =
    describe "projectPlane"
        [ test "Project (1,1,1) onto the X,Y plane" <|
            \() ->
                let
                    proj =
                        Geometry.projectPlane V3.i V3.j (V3.vec3 1 1 1)

                    crossX =
                        V3.cross V3.i proj

                    crossY =
                        V3.cross V3.j proj

                    dotX =
                        V3.dot crossX

                    dotY =
                        V3.dot crossY
                in
                    Expect.all
                        [ (dotX >> Expect.equal 0)
                        , (dotY >> Expect.equal 0)
                        ]
                        proj

           , fuzz3 unitVec3 unitVec3 unitVec3 "Project vector onto plane" <|
               \v1 v2 u ->
                   let proj = Geometry.projectPlane v1 v2 u
                       cross1 = V3.cross v1 proj
                       cross2 = V3.cross v2 proj
                       dot1 = V3.dot cross1
                       dot2 = V3.dot cross2
                   in
                       Expect.all
                           [ (dot1 >> floatEqual 0)
                           , (dot2 >> floatEqual 0)
                           ]
                           proj
        ]

{-
orientUpright : Test
orientUpright =
    describe "Orientation.upright"
        [ test "Upright" <|
            \() ->
                let
                    o = Orientation.fromAngleAxis (pi/4) V3.k
                    upr = Orientation.rollUpright o
                in
                    Expect.all
                        [ (Expect.equal Orientation.initial)
                        ]
                        upr
        ]
-}
