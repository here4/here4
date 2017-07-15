module Object.Types exposing
    ( Load(..)
    )


import Appearance exposing (Appearance)
import Math.Vector3 exposing (Vec3)

type Load result
    = Loading result
    | Ready Appearance Vec3
