module Thing exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (Entity)

import Appearance exposing (..)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)

type alias Animated model msg =
    { update : msg -> model -> (model, Cmd msg)
    , things : model -> List Body
    , animate : Time -> model -> model
    , focus : model -> Maybe Focus
    }

type alias ThingModel = Dynamic

type CtrlMsg
    = Move Vec3

type alias ThingMsg = Dispatch CtrlMsg Dynamic

type alias Things =
    { methods : Animated ThingModel ThingMsg
    , model : ThingModel
    }

msgUnpack : Dispatch CtrlMsg Dynamic -> Dispatch CtrlMsg a
msgUnpack msg = case msg of
    Self m -> Self (Dynamic.unpack m)
    Down c -> Down c

msgPack : Dispatch CtrlMsg a -> Dispatch CtrlMsg Dynamic
msgPack msg = case msg of
    Self m -> Self (Dynamic.pack m)
    Down c -> Down c

packInit : (model, Cmd msg) -> (ThingModel, Cmd ThingMsg)
packInit (x, cmd) = (Dynamic.pack x, Cmd.map (Self << Dynamic.pack) cmd)

packUpdate : (Dispatch CtrlMsg msg -> model -> (model, Cmd (Dispatch CtrlMsg msg))) -> ThingMsg -> ThingModel -> (ThingModel, Cmd ThingMsg)
packUpdate f msg dyn =
    let (newModel, newCmdMsg) = f (msgUnpack msg) (Dynamic.unpack dyn)
    in (Dynamic.pack newModel, Cmd.map msgPack newCmdMsg)

packAnimate : (Time -> model -> model) -> Time -> ThingModel -> ThingModel
packAnimate f dt dyn = Dynamic.pack (f dt (Dynamic.unpack dyn))

packThings : (a -> List Body) -> ThingModel -> List Body
packThings f dyn = f (Dynamic.unpack dyn)

packFocus : (a -> Maybe Focus) -> ThingModel -> Maybe Focus
packFocus f dyn = f (Dynamic.unpack dyn)


packThingMethods : Animated model (Dispatch CtrlMsg msg) -> Animated ThingModel ThingMsg
packThingMethods { update, animate, things, focus } =
    { update = packUpdate update
    , animate = packAnimate animate
    , things = packThings things
    , focus = packFocus focus
    }

createThings : (model, Cmd (Dispatch CtrlMsg msg)) -> Animated model (Dispatch CtrlMsg msg) -> (Things, Cmd ThingMsg)
createThings (model, msg) methods =
    ( { methods = packThingMethods methods
      , model = Dynamic.pack model
      }
    , Cmd.map msgPack msg
    ) 

createThingsNoChildren : (model, Cmd msg) -> Animated model msg -> (Things, Cmd ThingMsg)
createThingsNoChildren (model, msg) methods =
    createThings (model, Cmd.map Self msg) { methods | update = updateSelf methods.update }

-- | Update helper for things with no children
updateSelf : (msg -> model -> (model, Cmd msg))
    -> Dispatch CtrlMsg msg -> model -> (model, Cmd (Dispatch CtrlMsg msg))
updateSelf f msg model = case msg of
    Self selfMsg -> let (newModel, newMsg) = f selfMsg model in (newModel, Cmd.map Self newMsg)
    _            -> (model, Cmd.none)

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

things : Things -> List Body
things { methods, model } = methods.things model

focus : Things -> Maybe Focus
focus { methods, model } = methods.focus model

-- TODO: focus on a plane/surface/controls
type alias Focus = {
    pos : Vec3
}

type Body = BCtr Vec3 Vec3 Vec3 Appearance

type alias Visible a = { a | appear : Appearance }

type alias Oriented a = { a | scale : Vec3, pos : Vec3, orientation : Vec3 }
type alias Moving a = Oriented { a | velocity : Vec3 }
type alias Massive a = { a | mass : Float }
type alias Spherical a = { a | radius : Float }

extractBody : Oriented (Visible a) -> Body
extractBody x = BCtr x.scale x.pos x.orientation x.appear

tview : (Mat4 -> Mat4) -> Appearance -> Appearance
tview f appear p = appear { p | viewMatrix = f p.viewMatrix }

put : Vec3 -> Appearance -> Body
put pos appear = BCtr (vec3 1 1 1) pos (vec3 1 0 0) appear

place : Vec3 -> Body -> Body
place t (BCtr scale _ o s) = BCtr scale t o s

translate : Vec3 -> Body -> Body
translate t (BCtr scale p o s) = BCtr scale (V3.add t p) o s

resize : Float -> Body -> Body
resize scale (BCtr scale0 p o s) = BCtr (V3.scale scale scale0) p o s

thingToFocus : Body -> Focus
thingToFocus (BCtr _ p _ _) = { pos = p }

orientedToFocus : Oriented a -> Focus
orientedToFocus x = { pos = x.pos }


