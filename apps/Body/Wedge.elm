module Body.Wedge exposing (wedge)

import List exposing (concatMap, map)
import Time exposing (Time)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as V3 exposing (..)
import Math.Vector4 exposing (vec4)
import Math.Matrix4 as M4 exposing (..)
import WebGL exposing (..)
import Appearance exposing (..)
import Shaders.WorldVertex exposing (Vertex, worldVertex)
import Shaders.VoronoiDistances exposing (voronoiDistances)
import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)
import Model


wedge : Perception -> List Entity
wedge p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        s =
            p.globalTime

        detail =
            p.measuredFPS / 3.0

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0
    in
        [ entity worldVertex
            voronoiDistances
            topMesh
            { iResolution = resolution
            , iHMD = iHMD
            , iGlobalTime = s
            , iLensDistort = p.lensDistort
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , modelMatrix = M4.identity
            , viewPosition = p.cameraPos
            , lightPosition = p.lightPosition
            , ambientColor = p.ambientColor
            }
        , entity noiseVertex
            noiseColorFragment
            bottomMesh
            { iResolution = resolution
            , iHMD = iHMD
            , iDetail = detail
            , iGlobalTime = s
            , iGlobalTimeV = s
            , iLensDistort = p.lensDistort
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , modelMatrix = M4.identity
            , viewPosition = p.cameraPos
            , lightPosition = p.lightPosition
            , ambientColor = p.ambientColor
            }
        ]


topMesh : Mesh Vertex
topMesh =
    let
        white =
            vec4 1 1 1 1

        wHead =
            { position = vec3 0 0 1
            , normal = vec3 0 0 1
            , coord = vec3 0 0 0
            , color = white
            }

        wLB =
            { position = vec3 -0.6 0 -0.1
            , normal = V3.normalize (vec3 -0.6 0 0)
            , coord = vec3 -1 1 0
            , color = white
            }

        wCB =
            { position = vec3 0 0 -0.1
            , normal = vec3 0 0 -1
            , coord = vec3 0 1 0
            , color = white
            }

        wRB =
            { position = vec3 0.6 0 -0.1
            , normal = V3.normalize (vec3 0.6 0 0)
            , coord = vec3 1 1 0
            , color = white
            }

        wLT =
            { position = vec3 -0.3 0.2 0
            , normal = V3.normalize (vec3 -0.3 0.2 0)
            , coord = vec3 -0.5 1 0
            , color = white 
            }

        wRT =
            { position = vec3 0.3 0.2 0
            , normal = V3.normalize (vec3 0.3 0.2 0)
            , coord = vec3 0.5 1 0
            , color = white
            }

        -- Make a triangle joining wHead to v2 and v3
        headTriangle v2 v3 =
            let
                headNormal =
                    V3.scale 0.5 (V3.add v2.normal v3.normal)
            in
                ( { wHead | normal = headNormal }, v2, v3 )

        rearNormal =
            V3.cross
                (V3.sub wLT.position wLB.position)
                (V3.sub wCB.position wLB.position)

        rearTriangle v1 v2 v3 =
            ( { v1 | normal = rearNormal }
            , { v2 | normal = rearNormal }
            , { v3 | normal = rearNormal }
            )
    in
        triangles <|
            [ headTriangle wLB wLT
            , headTriangle wLT wRT
            , headTriangle wRT wRB
            , rearTriangle wLT wLB wCB
            , rearTriangle wLT wCB wRT
            , rearTriangle wRT wCB wRB
            ]


bottomMesh : Mesh NoiseVertex
bottomMesh =
    let
        white =
            vec4 0.1 0.1 0.7 1.7

        wHead =
            { position = vec3 0 0 1
            , normal = vec3 0 -1 0
            , color = white
            , coord = vec3 0 0 0
            , smoothing = 0.1
            , textureScale = 1.0
            , timeScale = 20.0
            }

        wLB =
            { position = vec3 -0.6 0 -0.1
            , normal = vec3 0 -1 0
            , color = white
            , coord = vec3 -1 1 0
            , smoothing = 0.1
            , textureScale = 1.0
            , timeScale = 20.0
            }

        wRB =
            { position = vec3 0.6 0 -0.1
            , normal = vec3 0 -1 0
            , color = white
            , coord = vec3 1 1 0
            , smoothing = 0.1
            , textureScale = 1.0
            , timeScale = 20.0
            }
    in
        triangles <| [ ( wHead, wLB, wRB ) ]
