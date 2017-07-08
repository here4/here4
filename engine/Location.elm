module Location exposing (..)

import Math.Vector3 exposing (Vec3)
import Orientation exposing (Orientation)

type alias AppId = String

type Location
    = At Vec3 OrientationSpec
    | Facing AppId
    | Behind AppId


type OrientationSpec
    = FacingNorth
    | WithOrientation Orientation
