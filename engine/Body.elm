module Body exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3

import Appearance exposing (Appearance)
import Orientation exposing (Orientation)


type Anchor = AnchorGround | AnchorSky | AnchorHUD

type alias Body =
    { anchor : Anchor
    , scale : Vec3
    , position : Vec3
    , orientation : Orientation
    , appear : Appearance
    }

-- BCtr Anchor Vec3 Vec3 Orientation Appearance

type alias Visible a = { a | appear : Appearance }

type alias Oriented a = { a | scale : Vec3, pos : Vec3, orientation : Orientation }
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
toBody x =
    { anchor = AnchorGround
    , scale = x.scale
    , position = x.pos
    , orientation = x.orientation
    , appear = x.appear
    }

-- | Reposition a Body
reposition : Vec3 -> Body -> Body
-- reposition t (BCtr anchor scale _ o s) = BCtr anchor scale t o s
reposition t body = { body | position = t }

-- | Resize a Body
resize : Float -> Body -> Body
-- resize scale (BCtr anchor scale0 p o s) = BCtr anchor (V3.scale scale scale0) p o s
resize scale body = { body | scale = V3.scale scale body.scale }

-- | Translate a Body
translate : Vec3 -> Body -> Body
-- translate t (BCtr anchor scale p o s) = BCtr anchor scale (V3.add t p) o s
translate t body = { body | position = V3.add t body.position }

put : Vec3 -> Appearance -> Body
-- put pos appear = BCtr AnchorGround (vec3 1 1 1) pos Orientation.initial appear
put pos appear =
    { anchor = AnchorGround
    , scale = vec3 1 1 1
    , position = pos
    , orientation = Orientation.initial
    , appear = appear
    }

anchorSky : Body -> Body
-- anchorSky (BCtr _ scale p o s) = BCtr AnchorSky scale p o s
anchorSky body = { body | anchor = AnchorSky }

bodyCamera : Body -> Camera
-- bodyCamera (BCtr _ _ p o _) =
bodyCamera body = 
    { position = body.position
    , orientation = Orientation.rotateBodyV body.orientation V3.k
    }
