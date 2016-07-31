module Main exposing (main)

import Bag exposing (Bag)
import Math.Vector3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (..)

import App
import Dynamic exposing (Dynamic)
import Model exposing (Args)

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
-- import Things.Ground exposing (renderGround)
import Things.Surface2D exposing (Placement, defaultPlacement)
import Things.Terrain as Terrain

type WorldMsg
    = TextureError Error
    | TextureLoaded Texture
    | TerrainGenerated Terrain
    | ThingMessage Bag.Key Dynamic

toThingMessage : Bag.Key -> ThingMsg -> WorldMsg
toThingMessage key (TMsg dyn) = ThingMessage key dyn

type alias WorldModel =
    { maybeTexture : Maybe Texture
    , maybeTerrain : Maybe Terrain
    , maybeSkybox : Maybe Thing
    , thingsBag : Bag Things
    }

main : Program Args
main =
  App.programWithFlags
    { init = worldInit 
    , view = worldView
    , update = worldUpdate
    , animate = worldAnimate
    , terrain = worldTerrain
    }

worldTerrain : WorldModel -> Maybe Terrain
worldTerrain model = model.maybeTerrain

worldSkybox : WorldModel -> Maybe Thing
worldSkybox model = model.maybeSkybox

worldInit : (WorldModel, Cmd WorldMsg)
worldInit =
    let (boidsModel, boidsCmdMsg) = Boids.create 100
        (ballsModel, ballsCmdMsg) = Balls.create 30
    in
    ( { maybeTexture = Nothing
      , maybeTerrain = Nothing
      , maybeSkybox = Nothing
      , thingsBag = Bag.empty
          |> Bag.insert boidsModel
          |> Bag.insert ballsModel
      }
    , Cmd.batch
        [ loadTexture "resources/woodCrate.jpg"
            |> Task.perform TextureError TextureLoaded
        , Terrain.generate TerrainGenerated defaultPlacement
        , Cmd.map (toThingMessage 7) boidsCmdMsg
        , Cmd.map (toThingMessage 8) ballsCmdMsg
        ]
    )

worldView : WorldModel -> Maybe Model.World
worldView model =
    case (model.maybeTexture, model.maybeTerrain, model.maybeSkybox) of
        (Nothing, _, _) -> Nothing
        (_, Nothing, _) -> Nothing
        (_, _, Nothing) -> Nothing
        (Just texture, Just terrain, Just skybox) ->
            Just (demoWorld texture terrain skybox model)

demoWorld : WebGL.Texture -> Terrain -> Thing -> WorldModel -> Model.World
demoWorld texture terrain skybox model =
    let
        myThings = List.concatMap Thing.things (Bag.items model.thingsBag)

        worldThings = myThings ++
            [ put (vec3 0 1.5 0) fogMountainsDiamond
            , put (vec3 5 1.5 1) cloudsDiamond
            , put (vec3 3 10 5) cloudsSphere
            , put (vec3 10 0 10) voronoiCube
            , put (vec3 -10 0 -10) skyCube -- fireCube
            , put (vec3 10 1.5 -10) fogMountainsCube
            , put (vec3 -2 0 -17) (textureCube texture)
            ]
    in
        { things = worldThings, terrain = terrain, skybox = skybox }

worldUpdate : WorldMsg -> WorldModel -> (WorldModel, Cmd WorldMsg)
worldUpdate msg model =
    case msg of
        ThingMessage key thingMsg ->
           case Bag.get key model.thingsBag of
               Nothing ->
                   ( model, Cmd.none )
               Just t ->
                   let (thingModel, thingCmdMsg) = Thing.update (TMsg thingMsg) t in
                  ( { model | thingsBag = Bag.replace key thingModel model.thingsBag }
                   , Cmd.map (toThingMessage key) thingCmdMsg
                   )
        TextureError err ->
            -- ( { model | message = "Error loading texture" }, Cmd.none )
            ( model, Cmd.none )
        TextureLoaded texture ->
            ( { model | maybeTexture = Just texture
                      , maybeSkybox = Just <| resize 80 <| put (vec3 0 1 1) skySphere
              }
            , Cmd.none )
        TerrainGenerated terrain ->
            ( { model | maybeTerrain = Just terrain }, Cmd.none )

worldAnimate : Time -> WorldModel -> WorldModel
worldAnimate dt model =
    { model | thingsBag = Bag.map (Thing.animate dt) model.thingsBag }

