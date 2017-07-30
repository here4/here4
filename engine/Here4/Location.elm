module Here4.Location exposing (..)

import Math.Vector3 exposing (Vec3)
import Here4.Orientation exposing (Orientation)


type alias AppId =
    String


type alias WorldId =
    String


type Location
    = Local Relative
    | Remote WorldId Relative


type Relative
    = At Vec3 OrientationSpec
    | Facing AppId
    | Behind AppId
    | Become AppId


type OrientationSpec
    = FacingNorth
    | WithOrientation Orientation


type Scale
    = Scale3 Float Float Float
    | Scale Float
    | Dimensions Float Float Float
    | Width Float
    | Height Float
    | Length Float


type Offset
    = ModelSpace Float Float Float
    | WorldSpace Float Float Float
    | Center
    | FloorCenter
