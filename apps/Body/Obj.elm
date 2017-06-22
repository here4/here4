module Body.Obj exposing (obj)

import Math.Vector3 exposing (..)
import Math.Matrix4 as M4 exposing (..)
import WebGL exposing (..)
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest
import Appearance exposing (..)
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

import Shaders.Reflection exposing (reflectionVert, reflectionFrag)

import OBJ
import OBJ.Types exposing (MeshWith, VertexWithTexture)

obj : MeshWith VertexWithTexture -> WebGL.Texture -> Appearance
obj { vertices, indices } texture p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0

        mesh = indexedTriangles vertices indices
    in
        [ entityWith [ DepthTest.default, cullFace front ]
            reflectionVert
            reflectionFrag
            mesh
            { camera = p.perspective, mvMat = p.lookAt, texture = texture }
{-
            { iResolution = resolution
            , iHMD = iHMD
            , iTexture = texture
            , iLensDistort = p.lensDistort
            , iPerspective = p.perspective
            , iLookAt = p.lookAt
            }
-}
        ]

