module Body exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3

import Appearance exposing (Appearance)
import Orientation exposing (Orientation)

type Body = BCtr Anchor Vec3 Vec3 Vec3 Appearance

type Anchor = AnchorGround | AnchorSky | AnchorHUD

type alias Visible a = { a | appear : Appearance }

type alias Oriented a = { a | scale : Vec3, pos : Vec3, orientation : Vec3 }
type alias Moving a = Oriented { a | velocity : Vec3 }
type alias Massive a = { a | mass : Float }
type alias Spherical a = { a | radius : Float }

-- TODO: define cameraAdd etc.
type alias Camera =
    { position : Vec3
    , orientation : Vec3
    }


-- | Use anything Oriented and Visible as a Body
toBody : Oriented (Visible a) -> Body
toBody x = BCtr AnchorGround x.scale x.pos x.orientation x.appear

-- | Reposition a Body
reposition : Vec3 -> Body -> Body
reposition t (BCtr anchor scale _ o s) = BCtr anchor scale t o s

-- | Resize a Body
resize : Float -> Body -> Body
resize scale (BCtr anchor scale0 p o s) = BCtr anchor (V3.scale scale scale0) p o s

-- | Translate a Body
translate : Vec3 -> Body -> Body
translate t (BCtr anchor scale p o s) = BCtr anchor scale (V3.add t p) o s

put : Vec3 -> Appearance -> Body
put pos appear = BCtr AnchorGround (vec3 1 1 1) pos (vec3 1 0 0) appear

anchorSky : Body -> Body
anchorSky (BCtr _ scale p o s) = BCtr AnchorSky scale p o s

bodyCamera : Body -> Camera
bodyCamera (BCtr _ _ p o _) = { position = p, orientation = o }
