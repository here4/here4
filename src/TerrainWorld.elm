module TerrainWorld exposing (create, TerrainModel, TerrainMsg)

import Control exposing (..)
import Dispatch exposing (..)
import Model exposing (Args)
import World exposing (..)

import App exposing (..)
import Ground exposing (Ground)


type alias TerrainWorldMsg = ()
type alias TerrainMsg = WorldMsg TerrainWorldMsg

type alias TerrainWorldModel = ()
type alias TerrainModel = WorldModel TerrainWorldModel

create : { apps : List (App, Cmd AppMsg) }
    -> Program Args (Model.Model TerrainModel) (Model.Msg TerrainMsg)
create details =
    World.create terrainInit terrainUpdate details

terrainInit : (TerrainWorldModel, Cmd TerrainWorldMsg)
terrainInit = ((), Cmd.none)

terrainUpdate : TerrainWorldMsg -> TerrainWorldModel -> (TerrainWorldModel, Cmd TerrainWorldMsg)
terrainUpdate msg model = (model, Cmd.none)
