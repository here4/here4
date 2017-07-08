module Body.Obj exposing (obj, obj2)

import Math.Vector3 exposing (..)
import Math.Matrix4 as M4 exposing (..)
import WebGL exposing (..)
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest
import Appearance exposing (..)
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)
import Shaders.Reflection as Shaders
import OBJ
import OBJ.Types as Obj exposing (VertexWithTexture)


obj : Obj.MeshWith VertexWithTexture -> WebGL.Texture -> Appearance
obj { vertices, indices } texture p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0

        mesh =
            indexedTriangles vertices indices
    in
        [ entityWith [ DepthTest.default, cullFace front ]
            Shaders.reflectionVert
            Shaders.reflectionFrag
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


obj2 : WebGL.Texture -> WebGL.Texture -> Obj.Mesh -> Appearance
obj2 textureDiff textureNorm mesh p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0

        -- some random lightpos, get this from the environment instead
        t =
            pi / 4

        lightPos =
            vec3 (0.5 * cos (2 * t)) (1 + 0.5 * sin (2 * t)) 0.5

        uniforms =
            { camera = p.perspective
            , mvMat = p.lookAt
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , modelMatrix = M4.identity
            , viewPosition = vec3 0 0 0
            , textureDiff = textureDiff
            , textureNorm = textureNorm
            , lightPosition = lightPos
            }
    in
        case mesh of
            Obj.WithoutTexture { vertices, indices } ->
                [ renderCullFace Shaders.simpleVert Shaders.simpleFrag (indexedTriangles vertices indices) uniforms ]

            Obj.WithTexture { vertices, indices } ->
                [ renderCullFace Shaders.noNormalVert Shaders.noNormalFrag (indexedTriangles vertices indices) uniforms ]

            Obj.WithTextureAndTangent { vertices, indices } ->
                [ renderCullFace Shaders.normalVert Shaders.normalFrag (indexedTriangles vertices indices) uniforms ]



{-
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
-}


renderCullFace : Shader a u v -> Shader {} u v -> WebGL.Mesh a -> u -> Entity
renderCullFace =
    entityWith [ DepthTest.default, cullFace front ]
