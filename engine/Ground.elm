module Ground
    exposing
        ( Ground
        )

import Math.Vector3 as V3 exposing (Vec3)


type alias Ground =
    { bounds : Vec3 -> Vec3
    , elevation : Vec3 -> Float
    }
