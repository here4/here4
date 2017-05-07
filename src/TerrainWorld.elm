module TerrainWorld exposing (create, WorldModel, TerrainWorldMsg)

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
import Placement exposing (defaultPlacement)
import Things.Terrain as Terrain

create : { apps : List (App, Cmd AppMsg) }
    -> Program Args (Model.Model WorldModel) (Model.Msg (WorldMsg TerrainWorldMsg))
create details =
  Space.programWithFlags
    { init = worldInit details
    , view = worldView
    , update = worldUpdate
    , focus = worldFocus
    , animate = worldAnimate
    , ground = worldGround
    }

type TerrainWorldMsg
    = TerrainGenerated Ground

type alias WorldModel =
    { maybeGround : Maybe Ground
    , apps : Bag App
    , focusKey : Maybe Bag.Key
    }

worldGround : WorldModel -> Maybe Ground
worldGround model = model.maybeGround

worldFocus : WorldModel -> Maybe Focus
worldFocus model = case Bag.items model.apps of
    (someitem :: _) -> App.focus someitem
    _ -> Nothing

worldApps : List (App, Cmd AppMsg) -> (Bag App, Cmd (WorldMsg a))
worldApps ts =
    let f (newApps, newCmdMsg) (oldBag, oldCmdMsgs) =
            let (key, newBag) = Bag.insert newApps oldBag
            in (newBag, oldCmdMsgs ++ [Cmd.map (Send key) newCmdMsg])
        (appsBag, unbatched) = List.foldl f (Bag.empty, []) ts
    in
        (appsBag, Cmd.batch unbatched)

worldInit : { apps : List (App, Cmd AppMsg) }
    -> (WorldModel, Cmd (WorldMsg TerrainWorldMsg))
worldInit details =
    let (appsBag, appCmds) = worldApps details.apps
    in
        ( { maybeGround = Nothing
          , apps = appsBag
          , focusKey = List.head (Bag.keys appsBag)
          }
        , Cmd.batch
            [ Terrain.generate (Hub << TerrainGenerated) defaultPlacement
            , appCmds
            ]
        )

worldView : WorldModel -> Maybe Model.World
worldView model =
    case model.maybeGround of
        Nothing     -> Nothing
        Just ground -> Just (makeWorld ground model)

makeWorld : Ground -> WorldModel -> Model.World
makeWorld ground model =
    let
        worldBodies = List.concatMap bodies (Bag.items model.apps)
    in
        { bodies = worldBodies, ground = ground }

worldUpdate : WorldMsg TerrainWorldMsg -> WorldModel -> (WorldModel, Cmd (WorldMsg TerrainWorldMsg))
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
                           let (appModel, appCmdMsg) = App.update (Down (Control.Move dp)) t
                           in
                               ( { model | apps = Bag.replace key appModel model.apps }
                               , Cmd.map (Send key) appCmdMsg
                               )

worldAnimate : Time -> WorldModel -> WorldModel
worldAnimate dt model =
    { model | apps = Bag.map (App.animate dt) model.apps }

