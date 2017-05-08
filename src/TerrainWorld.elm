module TerrainWorld exposing (create, TerrainModel, TerrainMsg)

import Bag exposing (Bag)
import Time exposing (Time)

import Space
import Control exposing (WorldMsg)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Model exposing (Args)

import Body exposing (Body)
import App exposing (..)
import Ground exposing (Ground)

type alias TerrainMsg = WorldMsg TerrainWorldMsg

create : ((Ground -> TerrainWorldMsg) -> Cmd (TerrainWorldMsg))
    -> { apps : List (App, Cmd AppMsg) }
    -> Program Args (Model.Model TerrainModel) (Model.Msg TerrainMsg)
create makeGround details =
    worldCreate (terrainInit makeGround) terrainGround terrainUpdate details

type TerrainWorldMsg
    = TerrainGenerated Ground

type alias TerrainWorldModel = Maybe Ground

type alias TerrainModel = WorldModel TerrainWorldModel

terrainGround : TerrainWorldModel -> Maybe Ground
terrainGround = identity

terrainInit : ((Ground -> TerrainWorldMsg) -> Cmd TerrainWorldMsg)
    -> (TerrainWorldModel, Cmd TerrainWorldMsg)
terrainInit makeGround = (Nothing, makeGround TerrainGenerated)

terrainUpdate : TerrainWorldMsg -> TerrainWorldModel -> (TerrainWorldModel, Cmd TerrainWorldMsg)
terrainUpdate msg model =
    case msg of
        TerrainGenerated terrain ->
            ( Just terrain, Cmd.none )

----------------------------------------------------------------------

type alias WorldModel a =
    { worldModel : a
    , apps : Bag App
    , focusKey : Maybe Bag.Key
    }

worldCreate :
       (model, Cmd msg)
    -> (model -> Maybe Ground)
    -> (msg -> model -> (model, Cmd msg))
    -> { apps : List (App, Cmd AppMsg) }
    -> Program Args (Model.Model (WorldModel model)) (Model.Msg (WorldMsg msg))
worldCreate hubInit hubGround hubUpdate details =
  Space.programWithFlags
    { init = worldInit hubInit details
    , view = worldView hubGround
    , update = worldUpdate hubUpdate
    , focus = worldFocus
    , animate = worldAnimate
    , ground = worldGround hubGround
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
          , apps = appsBag
          , focusKey = List.head (Bag.keys appsBag)
          }
        , Cmd.batch
            [ Cmd.map Hub hubCmd
            , appCmds
            ]
        )

worldView : (model -> Maybe Ground) -> WorldModel model -> Maybe Model.World
worldView hubGround model =
    case hubGround model.worldModel of
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

        Send key appMsg ->
           case Bag.get key model.apps of
               Nothing ->
                   ( model, Cmd.none )
               Just t ->
                   let (appModel, appCmdMsg) = App.update appMsg t
                   in
                       ( { model | apps = Bag.replace key appModel model.apps }
                       , Cmd.map (Send key) appCmdMsg
                       )
        Forward (Control.Move dp) ->
           case model.focusKey of
               Nothing ->
                   ( model, Cmd.none )
               Just key ->
                   case Bag.get key model.apps of
                       Nothing ->
                           ( model, Cmd.none )
                       Just t ->
                           let (appModel, appCmdMsg) = App.update (Ctrl (Control.Move dp)) t
                           in
                               ( { model | apps = Bag.replace key appModel model.apps }
                               , Cmd.map (Send key) appCmdMsg
                               )

worldAnimate : Time -> WorldModel a -> WorldModel a
worldAnimate dt model =
    { model | apps = Bag.map (App.animate dt) model.apps }

worldFocus : WorldModel a -> Maybe Focus
worldFocus model = case Bag.items model.apps of
    (someitem :: _) -> App.focus someitem
    _ -> Nothing

worldGround : (model -> Maybe Ground) -> WorldModel model -> Maybe Ground
worldGround hubGround model = hubGround model.worldModel

