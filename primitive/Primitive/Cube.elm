module Primitive.Cube exposing
    ( skyCube
    , textureCube
    , cloudsCube
    , fireCube
    , fogMountainsCube
    , plasmaCube
    , voronoiCube
    , cube
    , walls
    , floor
    , ceiling
    )

import Math.Vector3 exposing (..)
import Math.Matrix4 as M4 exposing (..)
import WebGL exposing (..)
import Appearance exposing (..)
import Shaders.Clouds exposing (clouds)
import Shaders.Sky exposing (sky)
import Shaders.Fire exposing (fire)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.SimplePlasma exposing (simplePlasma)
import Shaders.VoronoiDistances exposing (voronoiDistances)
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)


type alias Triple a =
    ( a, a, a )


skyCube : Perception -> List Entity
skyCube =
    cube worldVertex sky


cloudsCube : Perception -> List Entity
cloudsCube =
    cube worldVertex clouds


fireCube : Perception -> List Entity
fireCube =
    cube worldVertex fire


fogMountainsCube : Perception -> List Entity
fogMountainsCube =
    cube worldVertex fogMountains


plasmaCube : Perception -> List Entity
plasmaCube =
    cube worldVertex simplePlasma


voronoiCube : Perception -> List Entity
voronoiCube =
    cube worldVertex voronoiDistances



cube : Shader Vertex ShaderPerception a -> Shader {} ShaderPerception a -> Appearance
cube = render cubeMesh


walls : Shader Vertex ShaderPerception a -> Shader {} ShaderPerception a -> Appearance
walls = render wallsMesh


floor : Shader Vertex ShaderPerception a -> Shader {} ShaderPerception a -> Appearance
floor = render floorMesh


ceiling : Shader Vertex ShaderPerception a -> Shader {} ShaderPerception a -> Appearance
ceiling = render ceilingMesh


render : Mesh Vertex -> Shader Vertex ShaderPerception a -> Shader {} ShaderPerception a -> Appearance
render mesh vertexShader fragmentShader p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        s =
            p.globalTime

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0
    in
        [ entity vertexShader
            fragmentShader
            mesh
            { iResolution = resolution
            , iHMD = iHMD
            , iGlobalTime = s
            , iLensDistort = p.lensDistort
            , iPerspective = p.perspective
            , iLookAt = p.lookAt
            }
        ]


textureCube : WebGL.Texture -> Appearance
textureCube texture p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0
    in
        [ entity worldVertex
            textureFragment
            cubeMesh
            { iResolution = resolution
            , iHMD = iHMD
            , iTexture = texture
            , iLensDistort = p.lensDistort
            , iPerspective = p.perspective
            , iLookAt = p.lookAt
            }
        ]


cubeMesh : Mesh Vertex
cubeMesh =
    triangles <| List.concatMap rotatedFace [ ( 0, 0, 0 ), ( 90, 0, 1 ), ( 180, 0, 2 ), ( 270, 0, 3 ), ( 0, 90, 0 ), ( 0, -90, 0 ) ]


wallsMesh : Mesh Vertex
wallsMesh =
    triangles <| List.concatMap rotatedFace [ ( 0, 0, 0 ), ( 0, 90, 1 ), ( 0, 180, 2 ), ( 0, 270, 3 ) ]


floorMesh : Mesh Vertex
floorMesh =
    triangles <| List.concatMap rotatedFace [ ( 90, 0, 0 ) ]


ceilingMesh : Mesh Vertex
ceilingMesh =
    triangles <| List.concatMap rotatedFace [ ( -90, 0, 0 ) ]


rotatedFace : ( Float, Float, Float ) -> List (Triple Vertex)
rotatedFace ( angleX, angleY, coordX ) =
    let
        x =
            makeRotate (degrees angleX) (vec3 1 0 0)

        y =
            makeRotate (degrees angleY) (vec3 0 1 0)

        t =
            M4.transform (mul x (mul y (makeTranslate (vec3 0 0 0.5))))
                >> add (vec3 0 0.5 0)

        each f ( a, b, c ) =
            ( f a, f b, f c )
    in
        List.map (each (\x -> { x | pos = t x.pos, coord = add (vec3 coordX 0 0) x.coord })) face


face : List (Triple Vertex)
face =
    let
        white =
            vec3 1 1 1

        topLeft =
            -- { pos = vec3 -1 1 0, color = white, coord = vec3 0 1 0 }
            { pos = vec3 -0.5 0.5 0, color = white, coord = vec3 0 1 0 }

        topRight =
            -- { pos = vec3 1 1 0, color = white, coord = vec3 1 1 0 }
            { pos = vec3 0.5 0.5 0, color = white, coord = vec3 1 1 0 }

        bottomLeft =
            -- { pos = vec3 -1 -1 0, color = white, coord = vec3 0 0 0 }
            { pos = vec3 -0.5 -0.5 0, color = white, coord = vec3 0 0 0 }

        bottomRight =
            -- { pos = vec3 1 -1 0, color = white, coord = vec3 1 0 0 }
            { pos = vec3 0.5 -0.5 0, color = white, coord = vec3 1 0 0 }
    in
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]
