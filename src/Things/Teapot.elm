module Things.Teapot (jeep, teapot) where

import Math.Vector3 exposing (..)

-- import LoadObj exposing (loadObj, loadJeep)
import LoadObj exposing (loadObj)

teapot = Signal.map (\see -> { pos = vec3 0 0 0, orientation = vec3 0 0 1, see = see })
    (LoadObj.loadObj "resources/wt_teapot.obj")

jeep = Signal.map (\see -> { pos = vec3 0 0 0, orientation = vec3 0 0 1, see = see })
    (LoadObj.loadJeep "resources/Jeep.obj")
