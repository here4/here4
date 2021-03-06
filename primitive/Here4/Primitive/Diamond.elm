module Here4.Primitive.Diamond exposing (skyDiamond, cloudsDiamond, fogMountainsDiamond, diamond)

import Here4.Appearance exposing (..)
import List exposing (map2, repeat)
import Math.Vector3 as V3 exposing (..)
import Math.Vector4 exposing (vec4)
import Math.Matrix4 as M4 exposing (..)
import Shaders.Clouds exposing (clouds)
import Shaders.Sky exposing (sky)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.WorldVertex exposing (Vertex, worldVertex)
import WebGL exposing (..)


skyDiamond : Appearance
skyDiamond =
    diamond worldVertex sky


cloudsDiamond : Appearance
cloudsDiamond =
    diamond worldVertex clouds


fogMountainsDiamond : Appearance
fogMountainsDiamond =
    diamond worldVertex fogMountains


diamond : Shader Vertex ShaderPerception a -> Shader {} ShaderPerception a -> Appearance
diamond vertexShader fragmentShader p =
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
            diamondMesh
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



-- Make a triangle facet, with normals at all vertices pointing in the
-- same direction


makeFacet : Vertex -> Vertex -> Vertex -> ( Vertex, Vertex, Vertex )
makeFacet v1 v2 v3 =
    let
        p1 =
            v1.position

        p2 =
            v2.position

        p3 =
            v3.position

        normal =
            V3.cross (V3.sub p2 p1) (V3.sub p3 p1)

        setNormal v =
            { v | normal = normal }
    in
        ( setNormal v1, setNormal v2, setNormal v3 )


zipFacets : List Vertex -> List Vertex -> List Vertex -> List ( Vertex, Vertex, Vertex )
zipFacets xs ys zs =
    case ( xs, ys, zs ) of
        ( x :: xs1, y :: ys1, z :: zs1 ) ->
            makeFacet x y z :: zipFacets xs1 ys1 zs1

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
        , coord = M4.transform (rotZ n) x.coord
    }


seven : Vertex -> List Vertex
seven =
    unfold 7 (rotBoth 8)


eights : Vertex -> ( List Vertex, List Vertex )
eights x =
    let
        x7 =
            seven x
    in
        ( x :: x7, x7 ++ [ x ] )


diamondMesh : Mesh Vertex
diamondMesh =
    let
        white =
            vec4 1 1 1 1

        yOffset =
            1.21

        yMul =
            -4.2

        -- Vertices
        table0 =
            { position = vec3 0 0 0
            , normal = vec3 0 1 0
            , coord = vec3 0 (yMul * (0.0 - yOffset)) 0
            , color = white
            }

        tableV =
            { position = vec3 0.57 0 0
            , normal = vec3 0 1 0
            , coord = vec3 0 (yMul * (0.57 - yOffset)) 0
            , color = white
            }

        ( tableVS0, tableVS1 ) =
            eights tableV

        facetY =
            -0.2

        facet0 =
            rotBoth -16
                { position = vec3 0.8 facetY 0
                , normal = vec3 1 0 0 -- placeholder
                , coord = vec3 0.2 (yMul * (0.8 - yOffset)) 0
                , color = white
                }

        ( facetVS0, facetVS1 ) =
            eights facet0

        girdleY =
            -0.5

        girdleT0 =
            { position = vec3 1 girdleY 0
            , normal = vec3 1 0 0 -- placeholder
            , coord = vec3 0.3 (yMul * (0.9 - yOffset)) 0
            , color = white
            }

        ( girdleTS0, girdleTS1 ) =
            eights girdleT0

        girdleF0 =
            rotBoth 16 girdleT0

        girdleFS =
            girdleF0 :: seven girdleF0

        pavilionY =
            -1.3

        pavilionT0 =
            { position = vec3 0.2 pavilionY 0
            , normal = vec3 1 0 0 -- placeholder
            , coord = vec3 0.4 (yMul * (1.3 - yOffset)) 0
            , color = white
            }

        pavilionF0 =
            rotBoth -16 pavilionT0

        ( pavilionVS0, pavilionVS1 ) =
            eights pavilionF0

        cutlet =
            { position = vec3 0 -1.6 0
            , normal = vec3 1 0 0 -- placeholder
            , coord = vec3 0.41 (yMul * (0.87 - yOffset)) 0
            , color = white
            }

        --- Triangles
        mkTable v1 v2 =
            ( table0, v1, v2 )

        table =
            map2 mkTable tableVS1 tableVS0

        stars =
            zipFacets tableVS0 tableVS1 facetVS1

        bezelL =
            zipFacets facetVS0 tableVS0 girdleTS0

        bezelR =
            zipFacets facetVS1 girdleTS0 tableVS0

        upperGirdleL =
            zipFacets girdleTS0 facetVS1 girdleFS

        upperGirdleR =
            zipFacets girdleFS facetVS1 girdleTS1

        lowerGirdleL =
            zipFacets girdleTS0 girdleFS pavilionVS1

        lowerGirdleR =
            zipFacets girdleFS pavilionVS1 girdleTS1

        pavilionFacetL =
            zipFacets pavilionVS0 girdleTS0 (repeat 8 cutlet)

        pavilionFacetR =
            zipFacets girdleTS0 pavilionVS1 (repeat 8 cutlet)
    in
        triangles <|
            table
                ++ stars
                ++ bezelL
                ++ bezelR
                ++ upperGirdleL
                ++ upperGirdleR
                ++ lowerGirdleL
                ++ lowerGirdleR
                ++ pavilionFacetL
                ++ pavilionFacetR
                ++ []
