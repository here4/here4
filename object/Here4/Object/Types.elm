module Here4.Object.Types
    exposing
        ( Load(..)
        )

import Here4.Appearance exposing (Appearance)
import Math.Vector3 exposing (Vec3)


type Load result
    = Loading result
    | Ready Appearance Vec3
