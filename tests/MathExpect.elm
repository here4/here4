module MathExpect
    exposing
        ( all_
        , floatEqual
        , vec3Equal
        , vec3Collinear
        )

import Expect exposing (Expectation)
import Math.Vector3 as V3 exposing (Vec3, getX, getY, getZ)


{- | A variant of Expect.all that does not take an argument.
   Rather, the expectations take their arguments from the enclosing
   scope. This allows them to take different arguments, eg. to
   check each component of a tuple.
-}


all_ : List Expectation -> Expectation
all_ es =
    Expect.all (List.map always es) 0


floatRelativeTolerance : Float -> Float -> Float -> Bool
floatRelativeTolerance tolerance a b =
    if (a == b || (isNaN a && isNaN b)) then
        True
    else if (abs a < 10 * tolerance) then
        abs (a - b) < (10 * tolerance)
    else if (abs b < 10 * tolerance) then
        abs (a - b) < (10 * tolerance)
    else
        abs ((a - b) / a) < tolerance


equateWith : (a -> a -> String) -> (a -> a -> Bool) -> a -> a -> Expectation
equateWith failString f a b =
    if f a b then
        Expect.pass
    else
        Expect.fail (failString a b)


renderResult : String -> a -> a -> String
renderResult reason a b =
    toString a ++ "\n╷\n│ " ++ reason ++ "\n╵\n" ++ toString b


floatEqual : Float -> Float -> Expectation
floatEqual =
    let
        tolerance =
            0.01
    in
        equateWith
            (renderResult ("floats not within tolerance " ++ toString tolerance))
            (floatRelativeTolerance tolerance)


vec3ComponentRelativeTolerance : Float -> Vec3 -> Vec3 -> Bool
vec3ComponentRelativeTolerance tolerance a b =
    floatRelativeTolerance tolerance (V3.getX a) (V3.getX b)
        && floatRelativeTolerance tolerance (V3.getY a) (V3.getY b)
        && floatRelativeTolerance tolerance (V3.getZ a) (V3.getZ b)


vec3Equal : Vec3 -> Vec3 -> Expectation
vec3Equal =
    let
        tolerance =
            0.0001
    in
        equateWith
            (renderResult ("Components not within tolerance " ++ toString tolerance))
            (vec3ComponentRelativeTolerance tolerance)


vec3Collinear : Vec3 -> Vec3 -> Expectation
vec3Collinear u v =
    vec3Equal (V3.normalize u) (V3.normalize v)
