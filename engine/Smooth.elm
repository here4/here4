module Smooth exposing (..)

import List.Extra as List


{-
   convolve : List Float -> List Float -> List Float
   convolve coeffs input =
           List.foldl (+) 0 (List.zipWith (*) coeffs input)
-}
-- Smooth a list of coefficients with an input signal
-- Assume the input signal is already reversed, such
-- that it can be built by prepending new elements


smooth : (a -> a -> a) -> (Float -> a -> a) -> a -> List Float -> List a -> a
smooth sum scale pad coeffs input =
    List.foldl1 sum (padZipWith scale pad coeffs input)
        |> Maybe.withDefault pad


convolve : List Float -> List Float -> Float
convolve coeffs input =
    smooth (+) (*) 0 coeffs (List.reverse input)


padZipWith : (Float -> a -> a) -> a -> List Float -> List a -> List a
padZipWith scale pad coeffs input =
    case ( coeffs, input ) of
        ( c :: cs, i :: is ) ->
            scale c i :: padZipWith scale i cs is

        ( c :: cs, [] ) ->
            scale c pad :: padZipWith scale pad cs []

        ( [], _ ) ->
            []
