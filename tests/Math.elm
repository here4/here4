module Math exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, string)

import Orientation
import Math.Vector3 as V3

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
        ] 
