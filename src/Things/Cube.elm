module Things.Cube exposing (skyCube, textureCube, cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube, cube)

import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

import Thing exposing (Perception)

import Shaders.Clouds exposing (clouds)
import Shaders.Sky exposing (sky)
import Shaders.Fire exposing (fire)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.SimplePlasma exposing (simplePlasma)
import Shaders.VoronoiDistances exposing (voronoiDistances)
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

type alias Triple a = (a,a,a)

skyCube : Perception -> List Renderable
skyCube = cube worldVertex sky

cloudsCube : Perception -> List Renderable
cloudsCube = cube worldVertex clouds

fireCube : Perception -> List Renderable
fireCube = cube worldVertex fire

fogMountainsCube : Perception -> List Renderable
fogMountainsCube = cube worldVertex fogMountains

plasmaCube : Perception -> List Renderable
plasmaCube = cube worldVertex simplePlasma

voronoiCube : Perception -> List Renderable
voronoiCube = cube worldVertex voronoiDistances

-- cube : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Renderable
cube vertexShader fragmentShader p =
    let resolution = vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0
        s = p.globalTime
        iHMD = if p.cameraVR then 1.0 else 0.0
    in
        [ render vertexShader fragmentShader mesh
            { iResolution = resolution, iHMD = iHMD, iGlobalTime = s
            , iLensDistort = p.lensDistort, view = p.viewMatrix } ]

-- textureCube : WebGL.Texture -> Mat4 -> WebGL.Renderable
textureCube texture p =
    let resolution = vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0
        iHMD = if p.cameraVR then 1.0 else 0.0
    in
        [ render worldVertex textureFragment mesh
            { iResolution = resolution, iHMD = iHMD, iTexture = texture
            , iLensDistort = p.lensDistort, view = p.viewMatrix } ]

{-| The mesh for a cube -}
mesh : Drawable Vertex
mesh = Triangle <| List.concatMap rotatedFace [ (0,0,0), (90,0,1), (180,0,2), (270,0,3), (0,90,0), (0,-90,0) ]

-- rotatedFace : (Float,Float) -> List ({ pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 })
rotatedFace : (Float,Float,Float) -> List (Triple Vertex)
rotatedFace (angleX,angleY,coordX) =
  let
    x = makeRotate (degrees angleX) (vec3 1 0 0)
    y = makeRotate (degrees angleY) (vec3 0 1 0)
    t = x `mul` y `mul` makeTranslate (vec3 0 0 1)
    each f (a,b,c) =
      (f a, f b, f c)
  in
    List.map (each (\x -> {x | pos = transform t x.pos, coord = vec3 coordX 0 0 `add` x.coord })) face


-- face : List ({ pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 })
face : List (Triple Vertex)
face =
  let
    white       = vec3 1 1 1
    topLeft     = { pos = vec3 -1  1 0, color = white, coord = vec3 0 1 0 }
    topRight    = { pos = vec3  1  1 0, color = white, coord = vec3 1 1 0 }
    bottomLeft  = { pos = vec3 -1 -1 0, color = white, coord = vec3 0 0 0 }
    bottomRight = { pos = vec3  1 -1 0, color = white, coord = vec3 1 0 0 }
  in
    [ (topLeft,topRight,bottomLeft)
    , (bottomLeft,topRight,bottomRight)
    ]
