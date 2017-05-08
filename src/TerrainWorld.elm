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

create : ((Ground -> TerrainMsg) -> Cmd (TerrainMsg))
    -> { apps : List (App, Cmd AppMsg) }
    -> Program Args (Model.Model TerrainModel) (Model.Msg TerrainMsg)
create makeGround details =
  Space.programWithFlags
    { init = worldInit makeGround details
    , view = worldView
    , update = worldUpdate
    , focus = worldFocus
    , animate = worldAnimate
    , ground = worldGround
    }

type TerrainWorldMsg
    = TerrainGenerated Ground

type alias WorldModel a =
    { maybeGround : a
    , apps : Bag App
    , focusKey : Maybe Bag.Key
    }

type alias TerrainModel = WorldModel (Maybe Ground)

worldGround : TerrainModel -> Maybe Ground
worldGround model = model.maybeGround

worldFocus : WorldModel a -> Maybe Focus
worldFocus model = case Bag.items model.apps of
    (someitem :: _) -> App.focus someitem
    _ -> Nothing

worldApps : List (App, Cmd AppMsg) -> (Bag App, Cmd (WorldMsg a))
worldApps appsList =
    let f (newApps, newCmdMsg) (oldBag, oldCmdMsgs) =
            let (key, newBag) = Bag.insert newApps oldBag
            in (newBag, oldCmdMsgs ++ [Cmd.map (Send key) newCmdMsg])
        (appsBag, unbatched) = List.foldl f (Bag.empty, []) appsList
    in
        (appsBag, Cmd.batch unbatched)

worldInit : ((Ground -> TerrainMsg) -> Cmd TerrainMsg)
    -> { apps : List (App, Cmd AppMsg) }
    -> (TerrainModel, Cmd TerrainMsg)
worldInit makeGround details =
    let (appsBag, appCmds) = worldApps details.apps
    in
        ( { maybeGround = Nothing
          , apps = appsBag
          , focusKey = List.head (Bag.keys appsBag)
          }
        , Cmd.batch
            [ makeGround (Hub << TerrainGenerated)
            , appCmds
            ]
        )

worldView : TerrainModel -> Maybe Model.World
worldView model =
    case model.maybeGround of
        Nothing     -> Nothing
        Just ground -> Just (makeWorld ground model)

makeWorld : Ground -> TerrainModel -> Model.World
makeWorld ground model =
    let
        worldBodies = List.concatMap bodies (Bag.items model.apps)
    in
        { bodies = worldBodies, ground = ground }

worldUpdate : TerrainMsg -> TerrainModel -> (TerrainModel, Cmd TerrainMsg)
worldUpdate msg model =
    case msg of
        Hub (TerrainGenerated terrain) ->
            ( { model | maybeGround = Just terrain }, Cmd.none )

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

