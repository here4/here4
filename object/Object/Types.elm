module Object.Types
    exposing
        ( Load(..)
        , Scale(..)
        , Offset(..)
        )

import Appearance exposing (Appearance)
import Math.Vector3 exposing (Vec3)


type Load result
    = Loading result
    | Ready Appearance Vec3


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
