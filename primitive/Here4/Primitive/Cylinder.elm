module Here4.Primitive.Cylinder exposing (textureCylinder, skyCylinder, cloudsCylinder, fogMountainsCylinder, cylinder)

import Here4.Appearance exposing (..)
import Here4.Body exposing (Oriented, Visible)
import Here4.Orientation as Orientation
import List exposing (drop, concat, map, map2)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (vec4)
import Math.Matrix4 as M4 exposing (..)
import Shaders.Clouds exposing (clouds)
import Shaders.Sky exposing (sky)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)
import WebGL exposing (..)


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
        { scale = vec3 1 1 1
        , position = vec3 0 0 0
        , orientation = Orientation.initial
        , appear = appear
        }


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
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , modelMatrix = M4.identity
            , viewPosition = p.cameraPos
            , lightPosition = p.lightPosition
            , ambientColor = p.ambientColor
            }
        ]


textureCylinder : WebGL.Texture -> Appearance
textureCylinder texture p =
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
            cylinderMesh
            { iResolution = resolution
            , iHMD = iHMD
            , iTexture = texture
            , iLensDistort = p.lensDistort
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , modelMatrix = M4.identity
            , viewPosition = p.cameraPos
            , lightPosition = p.lightPosition
            , ambientColor = p.ambientColor
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
        | position = M4.transform (rotY n) x.position
        , normal = M4.transform (rotY n) x.normal
        , coord = M4.transform (rotZ n) x.coord
    }


rotMercator : Float -> Vertex -> Vertex
rotMercator n v =
    { v
        | position = M4.transform (rotY n) v.position
        , normal = M4.transform (rotY n) v.normal
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
            vec4 1 1 1 1

        yOffset =
            0.5

        -- Vertices
        top0 =
            { position = vec3 0 0.5 0
            , normal = vec3 0 1 0
            , coord = vec3 0 (0.0 - yOffset) 0
            , color = white
            }

        topV =
            { position = vec3 1 0.5 0
            , normal = vec3 1 0 0
            , coord = vec3 0 (1.0 - yOffset) 0
            , color = white
            }

        ( topVS0, topVS1 ) =
            circlePair topV

        bottom0 =
            { position = vec3 0 -0.5 0
            , normal = vec3 0 -1 0
            , coord = vec3 0 (0.0 - yOffset) 0
            , color = white
            }

        bottomV =
            { position = vec3 1 -0.5 0
            , normal = vec3 1 0 0
            , coord = vec3 0 (1.0 - yOffset) 0
            , color = white
            }

        ( bottomVS0, bottomVS1 ) =
            circlePair bottomV

        --- Triangles
        mkTop v1 v2 =
            ( top0
            , { v1 | normal = top0.normal }
            , { v2 | normal = top0.normal }
            )

        top =
            map2 mkTop topVS1 topVS0

        mkBottom v1 v2 =
            ( bottom0
            , { v1 | normal = bottom0.normal }
            , { v2 | normal = bottom0.normal }
            )

        bottom =
            map2 mkBottom bottomVS1 bottomVS0

        sideL =
            zip3 topVS0 topVS1 bottomVS0

        sideR =
            zip3 bottomVS0 topVS1 bottomVS1
    in
        triangles <|
            top
                ++ bottom
                ++ sideL
                ++ sideR
