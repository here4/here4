module Body exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3

import Appearance exposing (Appearance)

type Body = BCtr Vec3 Vec3 Vec3 Appearance

type alias Visible a = { a | appear : Appearance }

type alias Oriented a = { a | scale : Vec3, pos : Vec3, orientation : Vec3 }
type alias Moving a = Oriented { a | velocity : Vec3 }
type alias Massive a = { a | mass : Float }
type alias Spherical a = { a | radius : Float }


-- | Use anything Oriented and Visible as a Body
toBody : Oriented (Visible a) -> Body
toBody x = BCtr x.scale x.pos x.orientation x.appear

-- | Reposition a Body
reposition : Vec3 -> Body -> Body
reposition t (BCtr scale _ o s) = BCtr scale t o s

-- | Resize a Body
resize : Float -> Body -> Body
resize scale (BCtr scale0 p o s) = BCtr (V3.scale scale scale0) p o s

-- | Translate a Body
translate : Vec3 -> Body -> Body
translate t (BCtr scale p o s) = BCtr scale (V3.add t p) o s

put : Vec3 -> Appearance -> Body
put pos appear = BCtr (vec3 1 1 1) pos (vec3 1 0 0) appear

