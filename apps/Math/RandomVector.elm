module Math.RandomVector exposing (randomVec3)

import Random
import Math.Vector3 exposing (Vec3, vec3)


{-
   http://mathworld.wolfram.com/SphericalCoordinates.html
-}


fromSpherical : Float -> Float -> Float -> Vec3
fromSpherical r theta phi =
    let
        x =
            r * cos theta * sin phi

        y =
            r * sin theta * sin phi

        z =
            r * cos theta
    in
        vec3 x y z



{- Generate a random vector with given length
   http://mathworld.wolfram.com/SpherePointPicking.html
-}


randomVec3 : Float -> Random.Generator Vec3
randomVec3 r =
    let
        fromUV u v =
            let
                theta =
                    2 * pi * u

                phi =
                    acos (2 * v - 1)
            in
                fromSpherical r theta phi
    in
        Random.map2 fromUV (Random.float 0.0 1.0) (Random.float 0.0 1.0)


randomUnitVec3 : Random.Generator Vec3
randomUnitVec3 =
    randomVec3 1


randomVec3s : Int -> Float -> Random.Generator (List Vec3)
randomVec3s n r =
    Random.list n (randomVec3 r)


randomVec3_ : Float -> Vec3
randomVec3_ r =
    let
        fromUV ( u, v ) =
            let
                theta =
                    2 * pi * u

                phi =
                    acos (2 * v - 1)
            in
                fromSpherical r theta phi
    in
        fromUV ( 0.3, 0.7 )


randomUnitVec3_ : Vec3
randomUnitVec3_ =
    randomVec3_ 1



{- Generate n random vectors with given length
   http://mathworld.wolfram.com/SpherePointPicking.html
-}


randomVec3s_ : Int -> Float -> List Vec3
randomVec3s_ n r =
    let
        fromUV ( u, v ) =
            let
                theta =
                    2 * pi * u

                phi =
                    acos (2 * v - 1)
            in
                fromSpherical r theta phi

        pairs xs0 =
            case xs0 of
                [] ->
                    []

                [ x ] ->
                    []

                x1 :: x2 :: xs ->
                    ( x1, x2 ) :: pairs xs
    in
        List.map fromUV (pairs (rampList (2 * n)))


rampList : Int -> List Float
rampList n =
    List.map (\x -> (toFloat x) / (toFloat n)) (List.range 1 n)
