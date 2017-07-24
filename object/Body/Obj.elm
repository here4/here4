module Body.Obj exposing (reflective, textured)

import Math.Vector3 as V3 exposing (..)
import Math.Matrix4 as M4 exposing (..)
import WebGL exposing (..)
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest
import Appearance exposing (..)
import Shaders.Obj as Shaders


reflective : WebGL.Mesh { a | normal : Vec3, position : Vec3 } -> WebGL.Texture -> Appearance
reflective mesh texture p =
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
            Shaders.reflectionVert
            Shaders.reflectionFrag
            mesh
            { camera = p.perspective
            , mvMat = p.lookAt
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , texture = texture
            }

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



-- textured : WebGL.Mesh a -> Shader a u v -> Shader {} u v -> WebGL.Texture -> WebGL.Texture -> Appearance


textured mesh vertexShader fragmentShader textureDiff textureNorm p =
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
        [ renderCullFace vertexShader fragmentShader mesh uniforms ]



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
