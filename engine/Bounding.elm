module Bounding
    exposing
        ( Bounding
        , Methods
        , Motion
        , inside
        , outside
        , emplace
        , bounce
        , bump
        )

import Math.Vector3 as V3 exposing (Vec3)
import Body exposing (..)

type alias Motion =
    { position : Vec3, velocity : Vec3 }

type alias Methods a =
    { inside : a -> Vec3 -> Bool
    , emplace : a -> Vec3 -> Vec3
    , bounce : a -> Float -> Motion -> Motion
    , bump : a -> Float -> Motion -> Motion
    }

type alias Bounding a =
    { methods : Methods a
    , model : a
    }

inside : Bounding a -> Vec3 -> Bool
inside a pos = a.methods.inside a.model pos

outside : Bounding a -> Vec3 -> Bool
outside a pos = not (a.methods.inside a.model pos)

emplace : Bounding a -> Vec3 -> Vec3
emplace a pos = a.methods.emplace a.model pos

bounce : Bounding a -> Spherical (Moving b) -> Spherical (Moving b)
bounce a body =
    let
        motion = { position = body.position, velocity = body.velocity }
        { position, velocity } = a.methods.bounce a.model body.radius motion
    in
        { body | position = position, velocity = velocity }

-- bump : Bounding a -> Spherical (Moving b) -> Spherical (Moving b)
bump a body =
    let
        motion = { position = body.position, velocity = body.velocity }
        { position, velocity } = a.methods.bump a.model body.radius motion
    in
        { body | position = position, velocity = velocity }
