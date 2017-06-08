module App exposing (App, AppMsg, create, createUncontrolled, Focus, animate, bodies, label, framing, noFraming, focus, update, appToFocus, orientedToFocus)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (Entity)
import Appearance exposing (..)
import Body exposing (..)
import Camera exposing (Framing, Shot)
import Control exposing (..)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Ground exposing (Ground)


type alias Animated model msg =
    { label : model -> String
    , update : msg -> model -> ( model, Cmd msg )
    , bodies : model -> List Body
    , animate : Ground -> Time -> model -> model
    , framing : model -> Maybe Framing
    , focus : model -> Maybe Focus
    }


type alias AppModel =
    Dynamic


type alias AppMsg =
    CtrlMsg Dynamic


type alias App =
    { methods : Animated AppModel AppMsg
    , model : AppModel
    }


msgUnpack : CtrlMsg Dynamic -> CtrlMsg a
msgUnpack msg =
    case msg of
        Self m ->
            Self (Dynamic.unpack m)

        Ctrl c ->
            Ctrl c

        Effect e ->
            Effect e


msgPack : CtrlMsg a -> CtrlMsg Dynamic
msgPack msg =
    case msg of
        Self m ->
            Self (Dynamic.pack m)

        Ctrl c ->
            Ctrl c

        Effect e ->
            Effect e


packInit : ( model, Cmd msg ) -> ( AppModel, Cmd AppMsg )
packInit ( x, cmd ) =
    ( Dynamic.pack x, Cmd.map (Self << Dynamic.pack) cmd )


packLabel : (model -> String) -> AppModel -> String
packLabel f dyn =
    f (Dynamic.unpack dyn)


packUpdate : (CtrlMsg msg -> model -> ( model, Cmd (CtrlMsg msg) )) -> AppMsg -> AppModel -> ( AppModel, Cmd AppMsg )
packUpdate f msg dyn =
    let
        ( newModel, newCmdMsg ) =
            f (msgUnpack msg) (Dynamic.unpack dyn)
    in
        ( Dynamic.pack newModel, Cmd.map msgPack newCmdMsg )


packAnimate : (Ground -> Time -> model -> model) -> Ground -> Time -> AppModel -> AppModel
packAnimate f ground dt dyn =
    Dynamic.pack (f ground dt (Dynamic.unpack dyn))


packBodies : (a -> List Body) -> AppModel -> List Body
packBodies f dyn =
    f (Dynamic.unpack dyn)


packFraming : (a -> Maybe Framing) -> AppModel -> Maybe Framing
packFraming f dyn =
    f (Dynamic.unpack dyn)


packFocus : (a -> Maybe Focus) -> AppModel -> Maybe Focus
packFocus f dyn =
    f (Dynamic.unpack dyn)


packMethods : Animated model (CtrlMsg msg) -> Animated AppModel AppMsg
packMethods { label, update, animate, bodies, framing, focus } =
    { label = packLabel label
    , update = packUpdate update
    , animate = packAnimate animate
    , bodies = packBodies bodies
    , framing = packFraming framing
    , focus = packFocus focus
    }



-- | Create an app


create : ( model, Cmd (CtrlMsg msg) ) -> Animated model (CtrlMsg msg) -> ( App, Cmd AppMsg )
create ( model, msg ) methods =
    ( { methods = packMethods methods
      , model = Dynamic.pack model
      }
    , Cmd.map msgPack msg
    )



-- | Create an app that ignores control messages


createUncontrolled : ( model, Cmd msg ) -> Animated model msg -> ( App, Cmd AppMsg )
createUncontrolled ( model, msg ) methods =
    create ( model, Cmd.map Self msg ) { methods | update = updateSelf methods.update }



-- | Update helper for apps with no children


updateSelf :
    (msg -> model -> ( model, Cmd msg ))
    -> CtrlMsg msg
    -> model
    -> ( model, Cmd (CtrlMsg msg) )
updateSelf f msg model =
    case msg of
        Self selfMsg ->
            let
                ( newModel, newMsg ) =
                    f selfMsg model
            in
                ( newModel, Cmd.map Self newMsg )

        _ ->
            ( model, Cmd.none )



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


label : App -> String
label { methods, model } =
    methods.label model


update : AppMsg -> App -> ( App, Cmd AppMsg )
update msg { methods, model } =
    let
        ( newModel, newCmdMsg ) =
            methods.update msg model
    in
        ( { methods = methods, model = newModel }, newCmdMsg )



-- animate : Ground -> Time -> App -> (App, Cmd AppMsg)


animate : Ground -> Time -> App -> App
animate ground dt { methods, model } =
    let
        newModel =
            methods.animate ground dt model
    in
        { methods = methods, model = newModel }


bodies : App -> List Body
bodies { methods, model } =
    methods.bodies model


framing : App -> Maybe Framing
framing { methods, model } =
    methods.framing model


focus : App -> Maybe Focus
focus { methods, model } =
    methods.focus model



-- TODO: focus on a plane/surface/controls


type alias Focus =
    { position : Vec3
    }


appToFocus : Oriented a -> Focus
appToFocus body =
    { position = body.position }


orientedToFocus : Oriented a -> Focus
orientedToFocus x =
    { position = x.position }

noFraming : model -> Maybe Framing
noFraming _ = Nothing
