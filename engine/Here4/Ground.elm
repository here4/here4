module Here4.Ground
    exposing
        ( Ground
        , GroundSurface(..)
        , Floor
        , aboveSeaLevel
        , onWater
        , on
        )


import Math.Vector3 as V3 exposing (Vec3)


type GroundSurface
    = Snow
    | Grass
    | Beach
    | ShallowWater
    | DeepWater


type alias Floor =
    Vec3 -> Maybe Float


type alias Ground =
    { bounds : Float {- Radius -} -> Vec3 -> Vec3
    , elevation : Vec3 -> Float
    , surface : Vec3 -> GroundSurface
    , seaLevel : Float
    , coordRangeX : ( Float, Float )
    , coordRangeZ : ( Float, Float )
    }


-- eg. for use with AddApps.addSomewhere
aboveSeaLevel : Ground -> Vec3 -> Bool
aboveSeaLevel ground pos =
    ground.elevation pos > ground.seaLevel


onWater : Ground -> Vec3 -> Bool
onWater ground pos =
    ground.elevation pos <= ground.seaLevel


on : GroundSurface -> Ground -> Vec3 -> Bool
on surface ground pos =
    ground.surface pos == surface
