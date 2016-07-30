module Main exposing (main)

import Math.Vector3 exposing (Vec3, vec3)
import Random
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

import Boids exposing (Boids)

import Balls exposing (Balls)

import Things.Cube exposing (skyCube, textureCube, cloudsCube, fireCube, fogMountainsCube, voronoiCube)
import Things.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Things.Sphere exposing (skySphere, cloudsSphere)
import Things.Terrain exposing (Terrain)
import Things.Ground exposing (renderGround)
import Things.Surface2D exposing (Placement, defaultPlacement)
import Things.Terrain as Terrain

type WorldMsg
    = TextureError Error
    | TextureLoaded Texture
    | TerrainGenerated Terrain
    | ThingMessage ThingID String Dynamic

toThingMessage : ThingID -> ThingMsg -> WorldMsg
toThingMessage thingID (TMsg s dyn) = ThingMessage thingID s dyn

type alias WorldModel =
    { maybeTexture : Maybe Texture
    , maybeTerrain : Maybe Terrain
    , maybeSkybox : Maybe Thing
    , boids : Boids
    , balls : Balls
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
    let (boidsModel, boidsCmdMsg) = Boids.init 100
        (ballsModel, ballsCmdMsg) = Balls.init 30
    in
    ( { maybeTexture = Nothing
      , maybeTerrain = Nothing
      , maybeSkybox = Nothing
      , boids = boidsModel
      , balls = ballsModel
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
        boidThings = Boids.things model.boids
        ballThings = Balls.things model.balls

        worldThings = boidThings ++ ballThings ++
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
        ThingMessage thingID s dyn ->
            case thingID of
                7 -> let (boidsModel, boidsCmdMsg) = Boids.update (TMsg s dyn) model.boids in
                     ( { model | boids = boidsModel }, Cmd.map (toThingMessage 7) boidsCmdMsg )
                8 -> let (ballsModel, ballsCmdMsg) = Balls.update (TMsg s dyn) model.balls in
                     ( { model | balls = ballsModel }, Cmd.map (toThingMessage 8) ballsCmdMsg )
                _ -> ( model, Cmd.none )

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
    { model | boids = Boids.animate dt model.boids
            , balls = Balls.animate dt model.balls
    }

