module TerrainWorld exposing (create)

import Bag exposing (Bag)
import Math.Vector3 exposing (Vec3, vec3)
import Time exposing (Time)
import WebGL exposing (..)

import App
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Model exposing (Args, WorldCtrl)

import Thing exposing (..)
import Things.Surface2D exposing (defaultPlacement)
import Things.Terrain exposing (Terrain)
import Things.Terrain as Terrain

import Boids
import Balls

import Things.Cube exposing (skyCube, textureCube, cloudsCube, fireCube, fogMountainsCube, voronoiCube)
import Things.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Things.Sphere exposing (skySphere, cloudsSphere)
import Things.Terrain exposing (Terrain)
import Things.Surface2D exposing (Placement, defaultPlacement)
import Things.Terrain as Terrain

create : { things : List (Things, Cmd ThingMsg) , staticThings : List Thing, skybox : Thing }
    -> Program Args
create details =
  App.programWithFlags
    { init = worldInit details
    , view = worldView
    , update = worldUpdate
    , anyThing = worldAnything
    , focus = worldFocus
    , animate = worldAnimate
    , terrain = worldTerrain
    }

type MyWorldMsg
    = TerrainGenerated Terrain
    | Send Bag.Key (MyMsg Dynamic)

type alias WorldMsg = Dispatch WorldCtrl MyWorldMsg

type alias WorldModel =
    { maybeTerrain : Maybe Terrain
    , skybox : Thing
    , staticThings : List Thing
    , thingsBag : Bag Things
    }

worldTerrain : WorldModel -> Maybe Terrain
worldTerrain model = model.maybeTerrain

worldAnything : WorldModel -> Maybe Bag.Key
worldAnything model = case Bag.keys model.thingsBag of
    (somekey :: _) -> Just somekey
    _ -> Nothing

worldFocus : WorldModel -> Maybe Focus
worldFocus model = case Bag.items model.thingsBag of
    (someitem :: _) -> Thing.focus someitem
    _ -> Nothing

worldThings : List (Things, Cmd ThingMsg) -> (Bag Things, Cmd WorldMsg)
worldThings ts =
    let f (newThings, newCmdMsg) (oldBag, oldCmdMsgs) =
            let (key, newBag) = Bag.insert newThings oldBag
            in (newBag, oldCmdMsgs ++ [Cmd.map (Self << Send key) newCmdMsg])
        (bag, unbatched) = List.foldl f (Bag.empty, []) ts
    in
        (bag, Cmd.batch unbatched)

worldInit : { things : List (Things, Cmd ThingMsg) , staticThings : List Thing, skybox : Thing }
    -> (WorldModel, Cmd WorldMsg)
worldInit details =
    let (bag, thingCmds) = worldThings details.things
    in
        ( { maybeTerrain = Nothing
          , skybox = details.skybox
          , staticThings = details.staticThings
          , thingsBag = bag
          }
        , Cmd.batch
            [ Terrain.generate (Self << TerrainGenerated) defaultPlacement
            , thingCmds
            ]
        )

worldView : WorldModel -> Maybe Model.World
worldView model =
    case model.maybeTerrain of
        Nothing      -> Nothing
        Just terrain -> Just (makeWorld terrain model)

makeWorld : Terrain -> WorldModel -> Model.World
makeWorld terrain model =
    let
        myThings = List.concatMap Thing.things (Bag.items model.thingsBag)
        worldThings = myThings ++ model.staticThings
    in
        { things = worldThings, terrain = terrain, skybox = model.skybox }

worldUpdate : WorldMsg -> WorldModel -> (WorldModel, Cmd WorldMsg)
worldUpdate msg model =
    case msg of
        Self (Send key thingMsg) ->
           case Bag.get key model.thingsBag of
               Nothing ->
                   ( model, Cmd.none )
               Just t ->
                   let (thingModel, thingCmdMsg) = Thing.update thingMsg t
                   in
                       ( { model | thingsBag = Bag.replace key thingModel model.thingsBag }
                       , Cmd.map (Self << Send key) thingCmdMsg
                       )
        Self (TerrainGenerated terrain) ->
            ( { model | maybeTerrain = Just terrain }, Cmd.none )

        Down (Model.Move key dp) ->
           case Bag.get key model.thingsBag of
               Nothing ->
                   ( model, Cmd.none )
               Just t ->
                   let (thingModel, thingCmdMsg) = Thing.update (Ex (Thing.Move dp)) t
                   in
                       ( { model | thingsBag = Bag.replace key thingModel model.thingsBag }
                       , Cmd.map (Self << Send key) thingCmdMsg
                       )

worldAnimate : Time -> WorldModel -> WorldModel
worldAnimate dt model =
    { model | thingsBag = Bag.map (Thing.animate dt) model.thingsBag }

