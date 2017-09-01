module Here4.Ground
    exposing
        ( Ground
        )

import Math.Vector3 as V3 exposing (Vec3)


type alias Ground =
    { bounds : Float {- Radius -} -> Vec3 -> Vec3
    , elevation : Vec3 -> Float
    , seaLevel : Float
    , coordRangeX : (Float, Float)
    , coordRangeZ : (Float, Float)
    }
