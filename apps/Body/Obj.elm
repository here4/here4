module Body.Obj exposing (obj)

import Math.Vector3 exposing (..)
import Math.Matrix4 as M4 exposing (..)
import WebGL exposing (..)
import Appearance exposing (..)
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

import OBJ
import OBJ.Types exposing (MeshWith, VertexWithTexture)

type alias Triple a =
    ( a, a, a )


obj : MeshWith VertexWithTexture -> WebGL.Texture -> Appearance
obj mesh0 texture p =
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
            mesh
            { iResolution = resolution
            , iHMD = iHMD
            , iTexture = texture
            , iLensDistort = p.lensDistort
            , view = p.viewMatrix
            }
        ]


{-| The mesh for a cube
-}
mesh : Mesh Vertex
mesh =
    triangles <| List.concatMap rotatedFace [ ( 0, 0, 0 ), ( 90, 0, 1 ), ( 180, 0, 2 ), ( 270, 0, 3 ), ( 0, 90, 0 ), ( 0, -90, 0 ) ]


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
