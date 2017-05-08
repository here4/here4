module TerrainWorld exposing (create, TerrainModel, TerrainMsg)

import Control exposing (WorldMsg)
import Model exposing (Args)
import World exposing (..)

import App exposing (..)
import Ground exposing (Ground)


type TerrainWorldMsg = TerrainGenerated Ground
type alias TerrainMsg = WorldMsg TerrainWorldMsg

type alias TerrainWorldModel = Maybe Ground
type alias TerrainModel = WorldModel TerrainWorldModel

create : ((Ground -> TerrainWorldMsg) -> Cmd (TerrainWorldMsg))
    -> { apps : List (App, Cmd AppMsg) }
    -> Program Args (Model.Model TerrainModel) (Model.Msg TerrainMsg)
create makeGround details =
    World.create (terrainInit makeGround) terrainGround terrainUpdate details

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
