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
            , modelMatrix = M4.identity
            , viewPosition = p.cameraPos
            , lightPosition = p.lightPosition
            , ambientColor = p.ambientColor
            , texture = texture
            }
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

        uniforms =
            { camera = p.perspective
            , mvMat = p.lookAt
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , modelMatrix = M4.identity
            , viewPosition = p.cameraPos
            , textureDiff = textureDiff
            , textureNorm = textureNorm
            , ambientColor = p.ambientColor
            , lightPosition = p.lightPosition
            }
    in
        [ renderCullFace vertexShader fragmentShader mesh uniforms ]


renderCullFace : Shader a u v -> Shader {} u v -> WebGL.Mesh a -> u -> Entity
renderCullFace =
    entityWith [ DepthTest.default, cullFace front ]
