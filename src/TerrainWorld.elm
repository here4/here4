module TerrainWorld exposing (create, WorldModel, WorldMsg)

import Bag exposing (Bag)
import Time exposing (Time)

import Space
import Control exposing (CtrlMsg)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Model exposing (Args)

import Body exposing (Body)
import App exposing (..)
import Ground exposing (Ground)
import Placement exposing (defaultPlacement)
import Things.Terrain as Terrain

create : { things : List (App, Cmd AppMsg) , staticBodies : List Body, skybox : Body }
    -> Program Args (Model.Model WorldModel) (Model.Msg WorldMsg)
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
    | Send Bag.Key (CtrlMsg Dynamic)

type alias WorldMsg = CtrlMsg TerrainWorldMsg

type alias WorldModel =
    { maybeGround : Maybe Ground
    , skybox : Body
    , staticBodies : List Body
    , apps : Bag App
    , focusKey : Maybe Bag.Key
    }

worldGround : WorldModel -> Maybe Ground
worldGround model = model.maybeGround

worldFocus : WorldModel -> Maybe Focus
worldFocus model = case Bag.items model.apps of
    (someitem :: _) -> App.focus someitem
    _ -> Nothing

worldApps : List (App, Cmd AppMsg) -> (Bag App, Cmd WorldMsg)
worldApps ts =
    let f (newApps, newCmdMsg) (oldBag, oldCmdMsgs) =
            let (key, newBag) = Bag.insert newApps oldBag
            in (newBag, oldCmdMsgs ++ [Cmd.map (Self << Send key) newCmdMsg])
        (bag, unbatched) = List.foldl f (Bag.empty, []) ts
    in
        (bag, Cmd.batch unbatched)

worldInit : { things : List (App, Cmd AppMsg) , staticBodies : List Body, skybox : Body }
    -> (WorldModel, Cmd WorldMsg)
worldInit details =
    let (bag, thingCmds) = worldApps details.things
    in
        ( { maybeGround = Nothing
          , skybox = details.skybox
          , staticBodies = details.staticBodies
          , apps = bag
          , focusKey = List.head (Bag.keys bag)
          }
        , Cmd.batch
            [ Terrain.generate (Self << TerrainGenerated) defaultPlacement
            , thingCmds
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
        myBodies = List.concatMap bodies (Bag.items model.apps)
        worldBodies = myBodies ++ model.staticBodies
    in
        { things = worldBodies, ground = ground, skybox = model.skybox }

worldUpdate : WorldMsg -> WorldModel -> (WorldModel, Cmd WorldMsg)
worldUpdate msg model =
    case msg of
        Self (Send key thingMsg) ->
           case Bag.get key model.apps of
               Nothing ->
                   ( model, Cmd.none )
               Just t ->
                   let (thingModel, thingCmdMsg) = App.update thingMsg t
                   in
                       ( { model | apps = Bag.replace key thingModel model.apps }
                       , Cmd.map (Self << Send key) thingCmdMsg
                       )
        Self (TerrainGenerated terrain) ->
            ( { model | maybeGround = Just terrain }, Cmd.none )

        Down (Control.Move dp) ->
           case model.focusKey of
               Nothing ->
                   ( model, Cmd.none )
               Just key ->
                   case Bag.get key model.apps of
                       Nothing ->
                           ( model, Cmd.none )
                       Just t ->
                           let (thingModel, thingCmdMsg) = App.update (Down (Control.Move dp)) t
                           in
                               ( { model | apps = Bag.replace key thingModel model.apps }
                               , Cmd.map (Self << Send key) thingCmdMsg
                               )

worldAnimate : Time -> WorldModel -> WorldModel
worldAnimate dt model =
    { model | apps = Bag.map (App.animate dt) model.apps }

