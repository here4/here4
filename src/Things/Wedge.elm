module Things.Wedge (wedge) where

import List exposing (concatMap, map)
import Time exposing (Time)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (vec4)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

import Shaders.WorldVertex exposing (Vertex, worldVertex)
import Shaders.VoronoiDistances exposing (voronoiDistances)

import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)

import Model
import Engine exposing (..)

-- wedge : Oriented (Visible {})
wedge = Signal.constant <| { pos = vec3 0 0 0, orientation = vec3 1 0 1, appear = appearWedge }

appearWedge p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = p.globalTime
        detail = p.measuredFPS / 3.0
    in
        [ entity worldVertex voronoiDistances topMesh
            { iResolution=resolution, iGlobalTime=s
            , iLensDistort=p.lensDistort, view=p.viewMatrix }
        , entity noiseVertex noiseColorFragment bottomMesh
            { iResolution=resolution, iDetail=detail, iGlobalTime=s
            , iGlobalTimeV=s, iLensDistort=p.lensDistort, view=p.viewMatrix }
        ]

topMesh : Mesh Vertex
topMesh =
    let white = vec3 1 1 1
        wHead = { pos = vec3    0 0    1,   color = white, coord = vec3    0 0 0 }
        wLB   = { pos = vec3 -0.6 0   -0.1, color = white, coord = vec3   -1 1 0 }
        wCB   = { pos = vec3    0 0   -0.1, color = white, coord = vec3    0 1 0 }
        wRB   = { pos = vec3  0.6 0   -0.1, color = white, coord = vec3    1 1 0 }
        wLT   = { pos = vec3 -0.3 0.2  0,   color = white, coord = vec3 -0.5 1 0 }
        wRT   = { pos = vec3  0.3 0.2  0,   color = white, coord = vec3  0.5 1 0 }
    in
        Triangle <| [ (wHead, wLB, wLT), (wHead, wLT, wRT), (wHead, wRT, wRB)
                    , (wLT, wLB, wCB), (wLT, wCB, wRT), (wRT, wCB, wRB) ]

bottomMesh : Mesh NoiseVertex
bottomMesh =
    let white = vec4 0.1 0.1 0.7 1.7 
        wHead = { pos = vec3    0 0    1,   color = white, coord = vec3    0 0 0
                , smoothing = 0.1, textureScale = 1.0, timeScale = 20.0 }
        wLB   = { pos = vec3 -0.6 0   -0.1, color = white, coord = vec3   -1 1 0
                , smoothing = 0.1, textureScale = 1.0, timeScale = 20.0 }
        wRB   = { pos = vec3  0.6 0   -0.1, color = white, coord = vec3    1 1 0
                , smoothing = 0.1, textureScale = 1.0, timeScale = 20.0 }
    in
        Triangle <| [ (wHead, wLB, wRB) ]
