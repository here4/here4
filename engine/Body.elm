module Body exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3

import Appearance exposing (Appearance)
import Orientation exposing (Orientation)


type Anchor = AnchorGround | AnchorSky | AnchorHUD

{-
type alias Body =
    { anchor : Anchor
    , scale : Vec3
    , position : Vec3
    , orientation : Orientation
    , appear : Appearance
    }
-}

type alias Anchored a = { a | anchor : Anchor }

type alias Visible a = { a | appear : Appearance }

type alias Oriented a = { a | scale : Vec3, position : Vec3, orientation : Orientation }
type alias Moving a = Oriented { a | velocity : Vec3 }
type alias Massive a = { a | mass : Float }
type alias Spherical a = { a | radius : Float }

type alias HasBody a = Oriented (Visible (Anchored a))
type alias Body = HasBody {}

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
    , position = x.position
    , orientation = x.orientation
    , appear = x.appear
    }

-- | Reposition a Body
reposition : Vec3 -> Body -> Body
reposition t body = { body | position = t }

-- | Resize a Body
resize : Float -> Body -> Body
resize scale body = { body | scale = V3.scale scale body.scale }

-- | Translate a Body
translate : Vec3 -> Oriented a -> Oriented a
translate t body = { body | position = V3.add t body.position }

put : Vec3 -> Appearance -> Body
put position appear =
    { anchor = AnchorGround
    , scale = vec3 1 1 1
    , position = position
    , orientation = Orientation.initial
    , appear = appear
    }

anchorSky : Body -> Body
anchorSky body = { body | anchor = AnchorSky }

bodyCamera : Oriented a -> Camera
bodyCamera body = 
    { position = body.position
    , orientation = Orientation.rotateBodyV body.orientation V3.k
    }
