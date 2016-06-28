module View.Crate exposing (textureCube, cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube, cube)

import Time exposing (Time, inSeconds)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Window

import Shaders.Clouds exposing (clouds)
import Shaders.Fire exposing (fire)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.SimplePlasma exposing (simplePlasma)
import Shaders.VoronoiDistances exposing (voronoiDistances)
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

type alias Triple a = (a,a,a)

-- cloudsCube : Window.Size -> Time -> Mat4 -> Renderable
cloudsCube = cube worldVertex clouds

-- fireCube : Window.Size -> Time -> Mat4 -> Renderable
fireCube = cube worldVertex fire

-- fogMountainsCube : Window.Size -> Time -> Mat4 -> Renderable
fogMountainsCube = cube worldVertex fogMountains

-- plasmaCube : Window.Size -> Time -> Mat4 -> Renderable
plasmaCube = cube worldVertex simplePlasma

-- voronoiCube : Window.Size -> Time -> Mat4 -> Renderable
voronoiCube = cube worldVertex voronoiDistances

-- cube : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Renderable
cube vertexShader fragmentShader p =
    let resolution = vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0
        s = inSeconds p.globalTime
    in
        render vertexShader fragmentShader mesh
            { iResolution = resolution, iGlobalTime = s
            , iLensDistort = p.lensDistort, view = p.viewMatrix }

-- textureCube : WebGL.Texture -> Mat4 -> WebGL.Renderable
textureCube texture p =
    render worldVertex textureFragment mesh
        { iTexture = texture, iLensDistort = p.lensDistort, view = p.viewMatrix }

{-| The mesh for a cube -}
mesh : Drawable Vertex
mesh = Triangle <| List.concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]

-- rotatedFace : (Float,Float) -> List ({ pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 })
rotatedFace : (Float,Float) -> List (Triple Vertex)
rotatedFace (angleX,angleY) =
  let
    x = makeRotate (degrees angleX) (vec3 1 0 0)
    y = makeRotate (degrees angleY) (vec3 0 1 0)
    t = x `mul` y `mul` makeTranslate (vec3 0 0 1)
    each f (a,b,c) =
      (f a, f b, f c)
  in
    List.map (each (\x -> {x | pos = transform t x.pos })) face


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
