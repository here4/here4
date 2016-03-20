module Things.Teapot (teapot) where

import Math.Vector3 exposing (..)

import LoadObj exposing (loadObj)

teapot = Signal.map (\see -> { pos = vec3 0 0 0, orientation = vec3 0 0 1, see = see })
    (loadObj "resources/wt_teapot.obj")
