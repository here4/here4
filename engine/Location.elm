module Location exposing (..)

import Math.Vector3 exposing (Vec3)
import Orientation exposing (Orientation)


type Location
    = At Vec3 OrientationSpec
    | Near String


type OrientationSpec
    = FacingNorth
    | WithOrientation Orientation
