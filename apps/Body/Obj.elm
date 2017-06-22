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

type alias Triple a =
    ( a, a, a )


obj : MeshWith VertexWithTexture -> WebGL.Texture -> Appearance
obj mesh texture p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0
    in
        [ entityWith [ DepthTest.default, cullFace front ]
            reflectionVert
            reflectionFrag
            mesh
            -- { camera = camera, mvMat = modelView, texture = texture }
            { camera = p.perspective, mvMat = p.lookAt, texture = texture }
{-
            { iResolution = resolution
            , iHMD = iHMD
            , iTexture = texture
            , iLensDistort = p.lensDistort
            , view = p.viewMatrix
            }
-}
        ]

