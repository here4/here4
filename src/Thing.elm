module Thing exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (Renderable)
import Window

import Dynamic exposing (Dynamic)

type alias Animated model msg =
    { update : msg -> model -> (model, Cmd msg)
    , things : model -> List Thing
    , animate : Time -> model -> model
    , focus : model -> Maybe Focus
    }

type alias ThingModel = Dynamic

type CtrlMsg
    = Move Vec3

type MyMsg a
    = My a
    | Ex CtrlMsg

type alias ThingMsg = MyMsg Dynamic

type alias Things =
    { methods : Animated ThingModel ThingMsg
    , model : ThingModel
    }

msgUnpack : MyMsg Dynamic -> MyMsg a
msgUnpack msg = case msg of
    My m -> My (Dynamic.unpack m)
    Ex c -> Ex c

msgPack : MyMsg a -> MyMsg Dynamic
msgPack msg = case msg of
    My m -> My (Dynamic.pack m)
    Ex c -> Ex c

packInit : (model, Cmd msg) -> (ThingModel, Cmd ThingMsg)
packInit (x, cmd) = (Dynamic.pack x, Cmd.map (My << Dynamic.pack) cmd)

packUpdate : (MyMsg msg -> model -> (model, Cmd (MyMsg msg))) -> ThingMsg -> ThingModel -> (ThingModel, Cmd ThingMsg)
packUpdate f msg dyn =
    let (newModel, newCmdMsg) = f (msgUnpack msg) (Dynamic.unpack dyn)
    in (Dynamic.pack newModel, Cmd.map msgPack newCmdMsg)

packAnimate : (Time -> model -> model) -> Time -> ThingModel -> ThingModel
packAnimate f dt dyn = Dynamic.pack (f dt (Dynamic.unpack dyn))

packThings : (a -> List Thing) -> ThingModel -> List Thing
packThings f dyn = f (Dynamic.unpack dyn)

packFocus : (a -> Maybe Focus) -> ThingModel -> Maybe Focus
packFocus f dyn = f (Dynamic.unpack dyn)


packThingMethods : Animated model (MyMsg msg) -> Animated ThingModel ThingMsg
packThingMethods { update, animate, things, focus } =
    { update = packUpdate update
    , animate = packAnimate animate
    , things = packThings things
    , focus = packFocus focus
    }

createThings : (model, Cmd (MyMsg msg)) -> Animated model (MyMsg msg) -> (Things, Cmd ThingMsg)
createThings (model, msg) methods =
    ( { methods = packThingMethods methods
      , model = Dynamic.pack model
      }
    , Cmd.map msgPack msg
    ) 

{-
----------------------------------------------------------------------
-- Debugging: noop Things

update0 _ m = (m, Cmd.none)
animate0 dt t = t
things0 _ = []
methods0 = { update = update0, animate = animate0, things = things0 }

createThings0 : (model, Cmd msg) -> Things
createThings0 (model, msg) =
    { methods = methods0
    , model = Dynamic.pack model
    }
----------------------------------------------------------------------
-}

update : ThingMsg -> Things -> (Things, Cmd ThingMsg)
update msg { methods, model } =
    let (newModel, newCmdMsg) = methods.update msg model
    in ({ methods = methods, model = newModel }, newCmdMsg)

-- animate : Time -> Things -> (Things, Cmd ThingMsg)
animate : Time -> Things -> Things
animate dt { methods, model } =
    let newModel = methods.animate dt model
    in { methods = methods, model = newModel }

things : Things -> List Thing
things { methods, model } = methods.things model

focus : Things -> Maybe Focus
focus { methods, model } = methods.focus model

type alias Perception = {
    cameraPos  : Vec3,
    windowSize : Window.Size,
    globalTime : Time,
    viewMatrix : Mat4,
    lensDistort : Float,
    cameraVR   : Bool,
    measuredFPS : Float
}

-- TODO: focus on a plane/surface/controls
type alias Focus = {
    pos : Vec3
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

thingToFocus : Thing -> Focus
thingToFocus (Thing _ p _ _) = { pos = p }

orientedToFocus : Oriented a -> Focus
orientedToFocus x = { pos = x.pos }
