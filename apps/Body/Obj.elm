module Body.Obj exposing (reflective, textured)

import Math.Vector3 as V3 exposing (..)
import Math.Matrix4 as M4 exposing (..)
import WebGL exposing (..)
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest
import Appearance exposing (..)
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)
import Shaders.Obj as Shaders
import OBJ
import OBJ.Types as Obj exposing (VertexWithTexture)

import Debug

reflective : Obj.MeshWith VertexWithTexture -> WebGL.Texture -> Appearance
reflective { vertices, indices } texture p =
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


textured : Vec3 -> WebGL.Texture -> WebGL.Texture -> Obj.Mesh -> Appearance
textured offset textureDiff textureNorm mesh p =
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

{-
        debugBounds vertices =
            let
                f v (oldMinV, oldMaxV) =
                    let
                        (minX, minY, minZ) = V3.toTuple oldMinV
                        (maxX, maxY, maxZ) = V3.toTuple oldMaxV
                        (vx, vy, vz) = V3.toTuple v.position

                        minV = V3.fromTuple (min minX vx, min minY vy, min minZ vz)
                        maxV = V3.fromTuple (max maxX vx, max maxY vy, max maxZ vz)
                    in
                        (minV, maxV)

                big = 1e10

                bounds =
                    List.foldl f (vec3 big big big, vec3 -big -big -big)

                calcOffset (minV, maxV) =
                    add minV (V3.scale 0.5 (sub maxV minV))

                dvs vs =
                    let
                        tup =
                            (Debug.log "mesh bounds:" (bounds vs), vs)

                        tup2 =
                            (Debug.log "offsets:" (calcOffset ((bounds vs))), vs)
                    in
                        Tuple.second tup

            in
                dvs vertices
-}
            

        applyOffset =
            -- debugBounds >>
            List.map (\v -> { v | position = sub v.position offset })

    in
        case mesh of
            Obj.WithoutTexture { vertices, indices } ->
                [ renderCullFace Shaders.simpleVert Shaders.simpleFrag
                    (indexedTriangles (applyOffset vertices) indices) uniforms
                ]

            Obj.WithTexture { vertices, indices } ->
                [ renderCullFace Shaders.noNormalVert Shaders.noNormalFrag
                    (indexedTriangles (applyOffset vertices) indices) uniforms
                ]

            Obj.WithTextureAndTangent { vertices, indices } ->
                [ renderCullFace Shaders.normalVert Shaders.normalFrag
                    (indexedTriangles (applyOffset vertices) indices) uniforms
                ]


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
