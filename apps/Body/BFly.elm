module Body.BFly exposing (bfly)

import Appearance exposing (..)
import Body exposing (Oriented, Visible)
import GLSLPasta
import GLSLPasta.Core exposing (empty)
import GLSLPasta.Lighting exposing (vertex_clipPosition)
import GLSLPasta.Types as GLSLPasta exposing (Global(..), Dependencies(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4, vec4)
import Math.Matrix4 exposing (..)
import Orientation
import Time exposing (second)
import Shaders.VertexPasta exposing (..)
import WebGL exposing (..)


type alias BoidVertex =
    { pos : Vec3, color : Vec4, coord : Vec3, wing : Vec3 }

{-
type alias BoidShaderInput =
    { flapL : Mat4
    , flapR : Mat4
    , iGlobalTime : Float
    , iHMD : Float
    , iLensDistort : Float
    , iResolution : Vec3
    , iPerspective : Mat4
    , iLookAt : Mat4
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
            , iPerspective = p.perspective
            , iLookAt = p.lookAt
            , flapL = flapL
            , flapR = flapR
            }
        ]


mesh : Mesh BoidVertex
mesh =
    let
        white =
            vec4 1 1 1 1

        bHead =
            { pos = vec3 0 0 0.5, color = white, coord = vec3 0.5 0 0, wing = vec3 0 0 0 }

        bTail =
            { pos = vec3 0 0 -0.5, color = white, coord = vec3 0.5 1 0, wing = vec3 0 0 0 }

        bLeft =
            { pos = vec3 -0.7 0 -0.7, color = white, coord = vec3 0 0.5 0, wing = vec3 -1 0 0 }

        bRight =
            { pos = vec3 0.7 0 -0.7, color = white, coord = vec3 1 0.5 0, wing = vec3 1 0 0 }
    in
        triangles <| [ ( bHead, bTail, bLeft ), ( bHead, bTail, bRight ) ]


vertex_flap : GLSLPasta.Component
vertex_flap =
    { empty
        | id = "vertex_flap"
        , dependencies =
            Dependencies
                [ vertex_pos4
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
        pos4 *= flap;
"""
            ]
    }

bflyVertex : Shader BoidVertex { u | iLensDistort : Float, iPerspective : Mat4, iLookAt : Mat4, flapL : Mat4, flapR : Mat4 } { elm_FragColor : Vec4, elm_FragCoord : Vec2 , clipPosition : Vec4 }
bflyVertex =
    GLSLPasta.combine "bflyVertex"
        [ vertex_flap
        , perspective
        , vertex_elm_FragColor
        , vertex_elm_FragCoord
        , distort
        , vertex_clipPosition
        ]
    |> WebGL.unsafeShader
