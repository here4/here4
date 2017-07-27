module Body.BFly exposing (bfly)

import Appearance exposing (..)
import Body exposing (Oriented, Visible)
import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..), Dependencies(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4, vec4)
import Math.Matrix4 as M4 exposing (..)
import Orientation
import Time exposing (second)
import Shaders.VertexPasta exposing (..)
import WebGL exposing (..)


type alias BoidVertex =
    { position : Vec3
    , color : Vec4
    , coord : Vec3
    , wing : Vec3
    }



{-
   type alias BoidShaderInput =
       { flapL : Mat4
       , flapR : Mat4
       , iGlobalTime : Float
       , iHMD : Float
       , iLensDistort : Float
       , iResolution : Vec3
       , modelViewProjectionMatrix : Mat4
       }


   type alias BoidVertexShader =
       Shader BoidVertex BoidShaderInput
-}
-- bfly : Shader {} BoidShaderInput { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4 } -> Float -> Oriented (Visible {})


bfly fragmentShader f01 =
    makeBFly bflyVertex fragmentShader (f01 * second * pi * 2)



-- makeBFly : Shader BoidVertex BoidShaderInput a -> Shader {} BoidShaderInput a -> Float -> Oriented (Visible {})


makeBFly vertexShader fragmentShader flapStart =
    let
        appear =
            appearBFly vertexShader fragmentShader flapStart
    in
        { scale = vec3 1 1 1, position = (vec3 7 0 4), orientation = Orientation.initial, appear = appear }



-- appearBFly : Shader BoidVertex BoidShaderInput a -> Shader {} BoidShaderInput a -> Float -> Appearance


appearBFly vertexShader fragmentShader flapStart p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        s =
            p.globalTime + flapStart

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0

        -- s = log (show flapStart) <| (p.globalTime + flapStart)
        flap =
            -0.1 + (sin (s * 8) + 1) / 2

        flapL =
            makeRotate (-flap * 3 * pi / 8) (vec3 0 0 1)

        flapR =
            makeRotate (flap * 3 * pi / 8) (vec3 0 0 1)
    in
        [ entity vertexShader
            fragmentShader
            mesh
            { iResolution = resolution
            , iGlobalTime = s
            , iHMD = iHMD
            , iLensDistort = p.lensDistort
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , flapL = flapL
            , flapR = flapR
            , ambientColor = p.ambientColor
            , lightPosition = p.lightPosition
            }
        ]


mesh : Mesh BoidVertex
mesh =
    let
        white =
            vec4 1 1 1 1

        bHead =
            { position = vec3 0 0 0.5, color = white, coord = vec3 0.5 0 0, wing = vec3 0 0 0 }

        bTail =
            { position = vec3 0 0 -0.5, color = white, coord = vec3 0.5 1 0, wing = vec3 0 0 0 }

        bLeft =
            { position = vec3 -0.7 0 -0.7, color = white, coord = vec3 0 0.5 0, wing = vec3 -1 0 0 }

        bRight =
            { position = vec3 0.7 0 -0.7, color = white, coord = vec3 1 0.5 0, wing = vec3 1 0 0 }
    in
        triangles <| [ ( bHead, bTail, bLeft ), ( bHead, bTail, bRight ) ]


vertex_flap : GLSLPasta.Component
vertex_flap =
    { empty
        | id = "vertex_flap"
        , dependencies =
            Dependencies
                [ Lighting.vertex_position4
                ]
        , globals =
            [ Attribute "vec3" "wing"
            , Uniform "mat4" "flapL"
            , Uniform "mat4" "flapR"
            ]
        , splices =
            [ """
        mat4 flap;
        if (wing.x < 0.0) { flap = flapL; }
        else if (wing.x > 0.0) { flap = flapR; }
        else { flap = mat4(1.0); }
        position4 *= flap;
"""
            ]
    }


bflyVertex : Shader BoidVertex { u | iLensDistort : Float, modelViewProjectionMatrix : Mat4, flapL : Mat4, flapR : Mat4 } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4 }
bflyVertex =
    GLSLPasta.combine "bflyVertex"
        [ vertex_flap
        , Lighting.vertex_gl_Position
        , vertex_elm_FragColor
        , vertex_elm_FragCoord
        , distort
        , Lighting.vertex_clipPosition
        ]
        |> WebGL.unsafeShader
