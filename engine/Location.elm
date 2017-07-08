module Location exposing (..)

import Math.Vector3 exposing (Vec3)
import Orientation exposing (Orientation)

type alias AppId = String
type alias WorldId = String

type Location
    = Local Relative


type Relative
    = At Vec3 OrientationSpec
    | Facing AppId
    | Behind AppId
    | Become AppId


type OrientationSpec
    = FacingNorth
    | WithOrientation Orientation
