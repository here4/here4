module MathFuzz
    exposing
        ( vec3
        , nonZeroVec3
        , unitVec3
        , floatTuple3
        )

import Fuzz exposing (Fuzzer, float, floatRange)
import Fuzz exposing (map, andThen, constant)
import Math.Vector3 as V3 exposing (Vec3)


vec3 : Fuzzer Vec3
vec3 =
    Fuzz.map3 V3.vec3 float float float


nonZeroVec3 : Fuzzer Vec3
nonZeroVec3 =
    conditional
        { retries = 10
        , fallback = always V3.i
        , condition = \v -> v /= V3.vec3 0 0 0
        }
        vec3


unitVec3 : Fuzzer Vec3
unitVec3 =
    Fuzz.map V3.normalize nonZeroVec3


floatTuple3 : Fuzzer ( Float, Float, Float )
floatTuple3 =
    Fuzz.map3 (,,) float float float


floatTuple4 : Fuzzer ( Float, Float, Float, Float )
floatTuple4 =
    Fuzz.map4 (,,,) float float float float


floatRecord4 : Fuzzer { s : Float, i : Float, j : Float, k : Float }
floatRecord4 =
    Fuzz.map4 (\s i j k -> { s = s, i = i, j = j, k = k }) float float float float



-- Backported from elm-test 4.0.0, remove when bumping dependency


{-| Conditionally filter a fuzzer to remove occasional undesirable

  - input. Takes a limit for how many retries to attempt, and a
  - fallback
  - function to, if no acceptable input can be found, create one from
  - an
  - unacceptable one. Also takes a condition to determine if the
  - input is
  - acceptable or not, and finally the fuzzer itself.
  - A good number of max retires is ten. A large number of retries
  - might
  - blow the stack.

-}
conditional : { retries : Int, fallback : a -> a, condition : a -> Bool } -> Fuzzer a -> Fuzzer a
conditional { retries, fallback, condition } fuzzer =
    if retries <= 0 then
        map fallback fuzzer
    else
        fuzzer
            |> andThen
                (\val ->
                    if condition val then
                        constant val
                    else
                        conditional { retries = (retries - 1), fallback = fallback, condition = condition } fuzzer
                )
