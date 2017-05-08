module App exposing (App, AppMsg, create, createUncontrolled, Focus, animate, bodies, focus, update, appToFocus, orientedToFocus)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (Entity)

import Appearance exposing (..)
import Body exposing (..)
import Control exposing (..)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)

type alias Animated model msg =
    { update : msg -> model -> (model, Cmd msg)
    , bodies : model -> List Body
    , animate : Time -> model -> model
    , focus : model -> Maybe Focus
    }

type alias AppModel = Dynamic

type alias AppMsg = CtrlMsg Dynamic

type alias App =
    { methods : Animated AppModel AppMsg
    , model : AppModel
    }

msgUnpack : CtrlMsg Dynamic -> CtrlMsg a
msgUnpack msg = case msg of
    Self m -> Self (Dynamic.unpack m)
    Ctrl c -> Ctrl c
    Effect e -> Effect e

msgPack : CtrlMsg a -> CtrlMsg Dynamic
msgPack msg = case msg of
    Self m -> Self (Dynamic.pack m)
    Ctrl c -> Ctrl c
    Effect e -> Effect e

packInit : (model, Cmd msg) -> (AppModel, Cmd AppMsg)
packInit (x, cmd) = (Dynamic.pack x, Cmd.map (Self << Dynamic.pack) cmd)

packUpdate : (CtrlMsg msg -> model -> (model, Cmd (CtrlMsg msg))) -> AppMsg -> AppModel -> (AppModel, Cmd AppMsg)
packUpdate f msg dyn =
    let (newModel, newCmdMsg) = f (msgUnpack msg) (Dynamic.unpack dyn)
    in (Dynamic.pack newModel, Cmd.map msgPack newCmdMsg)

packAnimate : (Time -> model -> model) -> Time -> AppModel -> AppModel
packAnimate f dt dyn = Dynamic.pack (f dt (Dynamic.unpack dyn))

packApp : (a -> List Body) -> AppModel -> List Body
packApp f dyn = f (Dynamic.unpack dyn)

packFocus : (a -> Maybe Focus) -> AppModel -> Maybe Focus
packFocus f dyn = f (Dynamic.unpack dyn)


packMethods : Animated model (CtrlMsg msg) -> Animated AppModel AppMsg
packMethods { update, animate, bodies, focus } =
    { update = packUpdate update
    , animate = packAnimate animate
    , bodies = packApp bodies
    , focus = packFocus focus
    }

-- | Create an app
create : (model, Cmd (CtrlMsg msg)) -> Animated model (CtrlMsg msg) -> (App, Cmd AppMsg)
create (model, msg) methods =
    ( { methods = packMethods methods
      , model = Dynamic.pack model
      }
    , Cmd.map msgPack msg
    ) 

-- | Create an app that ignores control messages
createUncontrolled : (model, Cmd msg) -> Animated model msg -> (App, Cmd AppMsg)
createUncontrolled (model, msg) methods =
    create (model, Cmd.map Self msg) { methods | update = updateSelf methods.update }

-- | Update helper for apps with no children
updateSelf : (msg -> model -> (model, Cmd msg))
    -> CtrlMsg msg -> model -> (model, Cmd (CtrlMsg msg))
updateSelf f msg model = case msg of
    Self selfMsg -> let (newModel, newMsg) = f selfMsg model in (newModel, Cmd.map Self newMsg)
    _            -> (model, Cmd.none)

{-
----------------------------------------------------------------------
-- Debugging: noop App

update0 _ m = (m, Cmd.none)
animate0 dt t = t
bodies0 _ = []
methods0 = { update = update0, animate = animate0, bodies = bodies0 }

createApp0 : (model, Cmd msg) -> App
createApp0 (model, msg) =
    { methods = methods0
    , model = Dynamic.pack model
    }
----------------------------------------------------------------------
-}

update : AppMsg -> App -> (App, Cmd AppMsg)
update msg { methods, model } =
    let (newModel, newCmdMsg) = methods.update msg model
    in ({ methods = methods, model = newModel }, newCmdMsg)

-- animate : Time -> App -> (App, Cmd AppMsg)
animate : Time -> App -> App
animate dt { methods, model } =
    let newModel = methods.animate dt model
    in { methods = methods, model = newModel }

bodies : App -> List Body
bodies { methods, model } = methods.bodies model

focus : App -> Maybe Focus
focus { methods, model } = methods.focus model

-- TODO: focus on a plane/surface/controls
type alias Focus = {
    pos : Vec3
}

appToFocus : Body -> Focus
appToFocus (BCtr _ _ p _ _) = { pos = p }

orientedToFocus : Oriented a -> Focus
orientedToFocus x = { pos = x.pos }

