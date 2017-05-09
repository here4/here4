module World exposing (WorldModel, create)

import Bag exposing (Bag)
import Time exposing (Time)

import Space
import Control exposing (WorldMsg)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Model exposing (Args)

import Body exposing (Body, Camera)
import App exposing (..)
import Ground exposing (Ground)

type alias WorldModel a =
    { worldModel : a
    , maybeGround : Maybe Ground
    , apps : Bag App
    -- , focusKey : Maybe Bag.Key
    }

create :
       (model, Cmd msg)
    -> (msg -> model -> (model, Cmd msg))
    -> { apps : List (App, Cmd AppMsg) }
    -> Program Args (Model.Model (WorldModel model)) (Model.Msg (WorldMsg msg))
create hubInit hubUpdate details =
  Space.programWithFlags
    { init = worldInit hubInit details
    , view = worldView
    , update = worldUpdate hubUpdate
    , keyLimit = worldKeyLimit
    , animate = worldAnimate
    , camera = worldCamera
    , focus = worldFocus
    , ground = worldGround
    }

worldApps : List (App, Cmd AppMsg) -> (Bag App, Cmd (WorldMsg a))
worldApps appsList =
    let f (newApps, newCmdMsg) (oldBag, oldCmdMsgs) =
            let (key, newBag) = Bag.insert newApps oldBag
            in (newBag, oldCmdMsgs ++ [Cmd.map (Send key) newCmdMsg])
        (appsBag, unbatched) = List.foldl f (Bag.empty, []) appsList
    in
        (appsBag, Cmd.batch unbatched)

worldInit : (model, Cmd msg)
    -> { apps : List (App, Cmd AppMsg) }
    -> (WorldModel model, Cmd (WorldMsg msg))
worldInit hubInit details =
    let (hubModel, hubCmd) = hubInit
        (appsBag, appCmds) = worldApps details.apps
    in
        ( { worldModel = hubModel
          , maybeGround = Nothing
          , apps = appsBag
          -- , focusKey = List.head (Bag.keys appsBag)
          }
        , Cmd.batch
            [ Cmd.map Hub hubCmd
            , appCmds
            ]
        )

worldView : WorldModel model -> Maybe Model.World
worldView model =
    case model.maybeGround of
        Nothing     -> Nothing
        Just ground -> Just (makeWorld ground model)

makeWorld : Ground -> WorldModel a -> Model.World
makeWorld ground model =
    let
        worldBodies = List.concatMap bodies (Bag.items model.apps)
    in
        { bodies = worldBodies, ground = ground }

worldUpdate : (msg -> model -> (model, Cmd msg))
    -> WorldMsg msg -> WorldModel model -> (WorldModel model, Cmd (WorldMsg msg))
worldUpdate hubUpdate msg model =
    case msg of
        Hub hubMsg ->
            let (hubModel, hubCmd) = hubUpdate hubMsg model.worldModel
            in ( { model | worldModel = hubModel }, Cmd.map Hub hubCmd)

        HubEff (Control.UpdateGround ground) ->
            ( { model | maybeGround = Just ground }, Cmd.none)

        Send key appMsg ->
           case Bag.get key model.apps of
               Nothing ->
                   ( model, Cmd.none )
               Just t ->
                   let (appModel, appCmdMsg) = App.update appMsg t
                       foo x = case x of
                                 Effect e -> HubEff e
                                 m        -> Send key m
                   in
                       ( { model | apps = Bag.replace key appModel model.apps }
                       , Cmd.map foo appCmdMsg
                       )
        Forward key fwdMsg ->
           case Bag.get key model.apps of
               Nothing ->
                   ( model, Cmd.none )
               Just t ->
                   let (appModel, appCmdMsg) = App.update (Ctrl fwdMsg) t
                   in
                       ( { model | apps = Bag.replace key appModel model.apps }
                       , Cmd.map (Send key) appCmdMsg
                       )

worldKeyLimit : WorldModel a -> Int
worldKeyLimit model = Bag.size model.apps

worldAnimate : Time -> WorldModel a -> WorldModel a
worldAnimate dt model =
    { model | apps = Bag.map (App.animate dt) model.apps }

worldCamera : Maybe Bag.Key -> WorldModel a -> Maybe Camera
worldCamera mkey model = case mkey of
    Just key -> case Bag.get key model.apps of
                    Just app -> App.camera app
                    Nothing  -> Nothing
    Nothing -> Nothing

worldFocus : Bag.Key -> WorldModel a -> Maybe Focus
worldFocus key model = case Bag.get key model.apps of
    Just app -> App.focus app
    _        -> Nothing

worldGround : WorldModel model -> Maybe Ground
worldGround model = model.maybeGround

