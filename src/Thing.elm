module Thing exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (Renderable)
import Window

type alias Perception = {
    cameraPos  : Vec3,
    windowSize : Window.Size,
    globalTime : Time,
    viewMatrix : Mat4,
    lensDistort : Float,
    measuredFPS : Float
}

type alias See = Perception -> List Renderable

type Thing = Thing Vec3 Vec3 See

type alias Visible a = { a | see : See }

type alias Oriented a = { a | pos : Vec3, orientation : Vec3 }
type alias Moving a = Oriented { a | velocity : Vec3 }
type alias Massive a = { a | mass : Float }
type alias Spherical a = { a | radius : Float }

extractThing : Oriented (Visible a) -> Thing
extractThing x = Thing x.pos x.orientation x.see

tview : (Mat4 -> Mat4) -> See -> See
tview f see p = see { p | viewMatrix = f p.viewMatrix }

put : Vec3 -> See -> Thing
put pos see = Thing pos (vec3 0 1 0) see

place : Float -> Float -> Float -> Thing -> Thing
place x y z (Thing _ o s) = Thing (vec3 x y z) o s

