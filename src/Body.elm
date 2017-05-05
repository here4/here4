module Body exposing (..)

import Math.Vector3 exposing (Vec3, vec3)

import Appearance exposing (Appearance)

type Body = BCtr Vec3 Vec3 Vec3 Appearance

type alias Visible a = { a | appear : Appearance }

type alias Oriented a = { a | scale : Vec3, pos : Vec3, orientation : Vec3 }
type alias Moving a = Oriented { a | velocity : Vec3 }
type alias Massive a = { a | mass : Float }
type alias Spherical a = { a | radius : Float }
