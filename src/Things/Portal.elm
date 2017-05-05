module Things.Portal (cloudsPortal, firePortal, fogMountainsPortal, plasmaPortal, voronoiPortal, portal) where

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Time exposing (Time)
import WebGL exposing (..)

import Shaders.Clouds exposing (clouds)
import Shaders.Fire exposing (fire)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.SimplePlasma exposing (simplePlasma)
import Shaders.VoronoiDistances exposing (voronoiDistances)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

import Model
import Engine exposing (..)

import Char exposing (..)
import Keyboard exposing (isDown)

type alias Triple a = (a,a,a)

cloudsPortal = Signal.constant <| portal worldVertex clouds

firePortal = Signal.constant <| portal worldVertex fire

fogMountainsPortal = Signal.constant <| portal worldVertex fogMountains

plasmaPortal = Signal.constant <| portal worldVertex simplePlasma

voronoiPortal = Signal.constant <| portal worldVertex voronoiDistances

portal vertexShader fragmentShader =
    let appear = appearPortal vertexShader fragmentShader
    in { pos = (vec3 0 0 0), orientation = vec3 1 0 1, appear = appear }

-- portal : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> Perception -> Entity
appearPortal vertexShader fragmentShader p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = p.globalTime
    in
        [entity vertexShader fragmentShader mesh
            { iResolution=resolution, iGlobalTime=s
            , iLensDistort=p.lensDistort, view=p.viewMatrix }]

-- mesh : List (Triple Vertex)
mesh : Mesh Vertex
mesh = Triangle face
{-
mesh = concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]

rotatedFace : (Float,Float) -> [Triangle Vertex]
rotatedFace (angleXZ,angleYZ) =
  let x = makeRotate (degrees angleXZ) j
      y = makeRotate (degrees angleYZ) i
      t = x `mul` y
  in
      map (mapTriangle (\v -> {v | pos <- transform t v.pos })) face
-}

face : List (Triple Vertex)
face =
  let
      white       = vec3 1 1 1
      topLeft     = { pos = vec3 -1  1 1, color = white, coord = vec3 0 1 0 }
      topRight    = { pos = vec3  1  1 1, color = white, coord = vec3 1 1 0 }
      bottomLeft  = { pos = vec3 -1 -1 1, color = white, coord = vec3 0 0 0 }
      bottomRight = { pos = vec3  1 -1 1, color = white, coord = vec3 1 0 0 }
  in
      [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]
