module World exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Random
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (..)

import Thing exposing (..)
import Things.Surface2D exposing (defaultPlacement)
import Things.Terrain exposing (Terrain)
import Things.Terrain as Terrain

import Boids exposing (..)
import Behavior.Boids exposing (Boid, moveBoids)

import Balls exposing (..)
import Physics.Drop exposing (Drop, moveDrops)
import Physics.Collisions exposing (collisions)

import Things.Cube exposing (textureCube, fireCube, fogMountainsCube, voronoiCube)
import Things.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Things.Sphere exposing (cloudsSphere)
import Things.Terrain exposing (Terrain)
import Things.Ground exposing (renderGround)
import Things.Surface2D exposing (Placement, defaultPlacement)
import Things.Terrain as Terrain

import Things.BFly exposing (bfly)
import Shaders.VoronoiDistances exposing (voronoiDistances)

type WorldMsg
    = TextureError Error
    | TextureLoaded Texture
    | TerrainGenerated Terrain
    | BoidsGenerated (List (Boid (Visible {})))
    | BallsGenerated (List (Drop (Visible {})))

type alias WorldModel =
    { maybeTexture : Maybe Texture
    , maybeTerrain : Maybe Terrain
    , boids : List (Boid (Visible {}))
    , balls : List (Drop (Visible {}))
    }

world =
    { init = worldInit 
    , view = worldView
    , update = worldUpdate
    , animate = worldAnimate
    , terrain = worldTerrain
    }

worldTerrain : WorldModel -> Maybe Terrain
worldTerrain model = model.maybeTerrain

worldInit : (WorldModel, Cmd WorldMsg)
worldInit =
    ( { maybeTexture = Nothing
      , maybeTerrain = Nothing
      , boids = []
      , balls = []
      }
    , Cmd.batch
        [ loadTexture "resources/woodCrate.jpg"
            |> Task.perform TextureError TextureLoaded
        , Terrain.generate TerrainGenerated defaultPlacement
        , Random.generate BoidsGenerated (randomBoids 100)
        , Random.generate BallsGenerated (randomBalls 30)
        ]
    )

-- worldView : WorldModel -> Maybe Model.World
worldView model =
    case (model.maybeTexture, model.maybeTerrain) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just texture, Just terrain) ->
            Just (demoWorld texture terrain model)

-- demoWorld : WebGL.Texture -> Terrain -> WorldModel -> Model.World
demoWorld texture terrain model =
    let
        boidThings = List.map extractThing model.boids
        ballThings = List.map extractThing model.balls

        worldThings = boidThings ++ ballThings ++
            [ put (vec3 0 1.5 0) fogMountainsDiamond
            , put (vec3 5 1.5 1) cloudsDiamond
            , put (vec3 3 10 5) cloudsSphere
            , put (vec3 10 0 10) voronoiCube
            , put (vec3 -10 0 -10) fireCube
            , put (vec3 10 1.5 -10) fogMountainsCube
            , put (vec3 -2 0 -17) (textureCube texture)
            ]
    in
        { things = worldThings, terrain = terrain }

worldUpdate : WorldMsg -> WorldModel -> (WorldModel, Cmd WorldMsg)
worldUpdate msg model =
    case msg of
        TextureError err ->
            -- ( { model | message = "Error loading texture" }, Cmd.none )
            ( model, Cmd.none )
        TextureLoaded texture ->
            ( { model | maybeTexture = Just texture }, Cmd.none )
        TerrainGenerated terrain ->
            ( { model | maybeTerrain = Just terrain }, Cmd.none )
        BoidsGenerated boids ->
            ( { model | boids = boids }, Cmd.none )
        BallsGenerated balls ->
            ( { model | balls = balls }, Cmd.none )

worldAnimate : Time -> WorldModel -> WorldModel
worldAnimate dt model = 
    { model | boids = moveBoids dt model.boids
            , balls = collisions dt (moveDrops dt model.balls)
    }

