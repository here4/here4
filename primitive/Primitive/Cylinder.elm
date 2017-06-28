module Primitive.Cylinder exposing (skyCylinder, cloudsCylinder, fogMountainsCylinder, cylinder)

import List exposing (drop, concat, map, map2)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 as M4 exposing (..)
import WebGL exposing (..)
import Appearance exposing (..)
import Body exposing (Oriented, Visible)
import Orientation
import Shaders.Clouds exposing (clouds)
import Shaders.Sky exposing (sky)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.WorldVertex exposing (Vertex, worldVertex)


skyCylinder : Perception -> List Entity
skyCylinder =
    appearCylinder worldVertex sky


cloudsCylinder : Perception -> List Entity
cloudsCylinder =
    appearCylinder worldVertex clouds


fogMountainsCylinder : Oriented (Visible {})
fogMountainsCylinder =
    cylinder worldVertex fogMountains


cylinder : Shader Vertex ShaderPerception a -> Shader {} ShaderPerception a -> Oriented (Visible {})
cylinder vertexShader fragmentShader =
    let
        appear =
            appearCylinder vertexShader fragmentShader
    in
        { scale = vec3 1 1 1, position = vec3 0 0 0, orientation = Orientation.initial, appear = appear }


appearCylinder : Shader Vertex ShaderPerception a -> Shader {} ShaderPerception a -> Appearance
appearCylinder vertexShader fragmentShader p =
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
            cylinderMesh
            { iResolution = resolution
            , iGlobalTime = s
            , iHMD = iHMD
            , iLensDistort = p.lensDistort
            , iPerspective = p.perspective
            , iLookAt = p.lookAt
            }
        ]


unfold : Int -> (a -> a) -> a -> List a
unfold n f x =
    if n == 0 then
        []
    else
        let
            res =
                f x
        in
            (res :: unfold (n - 1) f res)


zip3 : List a -> List b -> List c -> List ( a, b, c )
zip3 xs ys zs =
    case ( xs, ys, zs ) of
        ( x :: xs1, y :: ys1, z :: zs1 ) ->
            ( x, y, z ) :: zip3 xs1 ys1 zs1

        _ ->
            []


rotY : Float -> Mat4
rotY n =
    makeRotate (2 * pi / n) (vec3 0 1 0)


rotZ : Float -> Mat4
rotZ n =
    makeRotate (-2 * pi / n) (vec3 0 0 1)


rotBoth : Float -> Vertex -> Vertex
rotBoth n x =
    { x
        | pos = M4.transform (rotY n) x.pos
        , coord = M4.transform (rotZ n) x.coord
    }


rotMercator : Float -> Vertex -> Vertex
rotMercator n v =
    { v
        | pos = M4.transform (rotY n) v.pos
        , coord = vec3 (getX v.coord + (1.0 / n)) (getY v.coord) 0
    }


circle : Vertex -> List Vertex
circle =
    unfold 31 (rotMercator 32)


circlePair : Vertex -> ( List Vertex, List Vertex )
circlePair x =
    let
        c =
            circle x
    in
        ( x :: c, c ++ [ x ] )


unfoldMercator : Int -> Vertex -> List Vertex
unfoldMercator n =
    unfold (n - 1) (rotMercator (toFloat n))


verticesMercator : Int -> Vertex -> ( List Vertex, List Vertex )
verticesMercator n x =
    let
        xs =
            unfoldMercator n x
    in
        ( x :: xs, xs ++ [ x ] )


cylinderMesh : Mesh Vertex
cylinderMesh =
    let
        white =
            vec3 1 1 1

        yOffset = 0.5

        -- Vertices
        top0 =
            { pos = vec3 0 0.5 0, color = white, coord = vec3 0 (0.0 - yOffset) 0 }

        topV =
            { pos = vec3 1 0.5 0, color = white, coord = vec3 0 (1.0- yOffset) 0 }

        ( topVS0, topVS1 ) =
            circlePair topV

        bottom0 =
            { pos = vec3 0 -0.5 0, color = white, coord = vec3 0 (0.0 - yOffset) 0 }

        bottomV =
            { pos = vec3 1 -0.5 0, color = white, coord = vec3 0 (1.0- yOffset) 0 }

        ( bottomVS0, bottomVS1 ) =
            circlePair bottomV

        --- Triangles
        mkTop v1 v2 =
            ( top0, v1, v2 )

        top =
            map2 mkTop topVS1 topVS0

        mkBottom v1 v2 =
            ( bottom0, v1, v2 )

        bottom =
            map2 mkBottom bottomVS1 bottomVS0

        sideL =
            zip3 topVS0 topVS1 bottomVS0

        sideR =
            zip3 bottomVS0 topVS1 bottomVS1

    in
        triangles <|
            top ++ bottom ++ sideL ++ sideR 
