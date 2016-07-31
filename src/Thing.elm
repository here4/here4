module Thing exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (Renderable)
import Window

import Dynamic exposing (Dynamic)

{-
type alias ThingModel model msg =
    { update : msg -> model -> (model, Cmd msg)
    , things : model -> List Thing
    }
-}

{-
type alias DynamicModel =
    { update : ThingMsg -> Dynamic -> (Dynamic, Cmd ThingMsg)
    , things : Dynamic -> 
    }
-}

-- type alias Animated a = ThingModel { a | animate : Time -> a -> a }
-- type alias Animated a = ThingModel { a | animate : Time -> a -> a }

type alias Animated model msg =
    { update : msg -> model -> (model, Cmd msg)
    , things : model -> List Thing
    , animate : Time -> model -> model
    }

type ThingModel = TM Dynamic
type ThingMsg = TMsg Dynamic

type alias Things =
    { methods : Animated ThingModel ThingMsg
    , model: ThingModel
    }

packInit : (model, Cmd msg) -> (ThingModel, Cmd ThingMsg)
packInit (x, cmd) = (TM (Dynamic.pack x), Cmd.map (TMsg << Dynamic.pack) cmd)

packUpdate : (msg -> model -> (model, Cmd msg)) -> ThingMsg -> ThingModel -> (ThingModel, Cmd ThingMsg)
packUpdate f (TMsg msg) (TM dyn) =
    let (newModel, newCmdMsg) = f (Dynamic.unpack msg) (Dynamic.unpack dyn)
    in (TM (Dynamic.pack newModel), Cmd.map (TMsg << Dynamic.pack) newCmdMsg)

packAnimate : (Time -> model -> model) -> Time -> ThingModel -> ThingModel
packAnimate f dt (TM dyn) = TM (Dynamic.pack (f dt (Dynamic.unpack dyn)))

packThings : (a -> List Thing) -> ThingModel -> List Thing
packThings f (TM dyn) = f (Dynamic.unpack dyn)


packThingMethods : Animated model msg -> Animated ThingModel ThingMsg
packThingMethods { update, animate, things } =
    { update = packUpdate update
    , animate = packAnimate animate
    , things = packThings things
    }

createThings : (model, Cmd msg) -> Animated model msg -> (Things, Cmd ThingMsg)
createThings (model, msg) methods =
    ( { methods = packThingMethods methods
      , model = TM (Dynamic.pack model)
      }
    , Cmd.map (TMsg << Dynamic.pack) msg
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
    , model = TM (Dynamic.pack model)
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

