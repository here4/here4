module Thing exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
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
    cameraVR   : Bool,
    measuredFPS : Float
}

type alias See = Perception -> List Renderable

type Thing = Thing Vec3 Vec3 Vec3 See

type alias Visible a = { a | see : See }

type alias Oriented a = { a | scale : Vec3, pos : Vec3, orientation : Vec3 }
type alias Moving a = Oriented { a | velocity : Vec3 }
type alias Massive a = { a | mass : Float }
type alias Spherical a = { a | radius : Float }

extractThing : Oriented (Visible a) -> Thing
extractThing x = Thing x.scale x.pos x.orientation x.see

tview : (Mat4 -> Mat4) -> See -> See
tview f see p = see { p | viewMatrix = f p.viewMatrix }

put : Vec3 -> See -> Thing
put pos see = Thing (vec3 1 1 1) pos (vec3 1 0 0) see

place : Vec3 -> Thing -> Thing
place t (Thing scale _ o s) = Thing scale t o s

translate : Vec3 -> Thing -> Thing
translate t (Thing scale p o s) = Thing scale (V3.add t p) o s

resize : Float -> Thing -> Thing
resize scale (Thing scale0 p o s) = Thing (V3.scale scale scale0) p o s

