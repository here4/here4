module Ground exposing
    ( Ground
    , bounds, elevation
    )

import Math.Vector3 as V3 exposing (Vec3, vec3, getX, getZ)

import Array2D exposing (Array2D)
import Body exposing (Body, Oriented, Visible, toBody)
import Placement exposing (Placement)

type alias Ground =
    { placement : Placement
    , elevations : Array2D Float
    , bodies : List Body
    }

----------------------------------------------------------------------

-- bounds : Placement -> Vec3 -> Vec3
bounds : Ground -> Vec3 -> Vec3
bounds { placement } pos =
    let bound x low high = if (x < low) then low else (if x > high then high else x)
        (x,y,z) = V3.toTuple pos
    -- in vec3 (bound x -246 1782) (bound y 0 1000) (bound z -246 1782)
    in vec3
       (bound x (placement.xOffset + 10) (placement.xOffset + toFloat placement.bigSide * placement.xDelta - 10))
       (bound y 0 1000)
       (bound z (placement.zOffset + 10) (placement.zOffset + toFloat placement.bigSide * placement.zDelta - 10))

-- Elevation of terrain at a given coordinate
-- Linearly interpolated on the mesh triangle
-- elevation : Placement -> Array2D Float -> Vec3 -> Float
elevation : Ground -> Vec3 -> Float
elevation { placement, elevations } pos =
    let
        ix0 = (getX pos + 256) / 2
        ix  = floor ix0
        ixf = ix0 - toFloat ix

        iz0 = (getZ pos + 256) / 2
        iz  = floor iz0
        izf = iz0 - toFloat iz

        getXZ x z = (Array2D.getXY x z 0 elevations) * placement.yMult

        i00 = getXZ ix     iz      --     00 ... 10  -> x
        i10 = getXZ (ix+1) iz      --  |  .    /  .
                                   --  v  .   /   .
        i01 = getXZ ix     (iz+1)  --     .  /    .
        i11 = getXZ (ix+1) (iz+1)  --  z  01 ... 11

        mix a b f = (a * (1-f) + b * f) / 2 -- f describes how close to a

    in
        if ixf + izf < 1.0 then
            mix i00 i10 ixf + mix i00 i01 izf
        else
            mix i01 i11 ixf + mix i10 i11 izf

            
