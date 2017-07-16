module Main exposing (main)

import Html exposing (Html)
import Location exposing (..)
import Math.Vector3 as V3 exposing (vec3)
import Navigator exposing (..)
import RAM

import Boids
import BoxRoom
import Balls
import Obj
import Object
import Object.Attributes exposing (..)
import Object.Types exposing (..)
import Orientation
import Shufflepuck
import Sky
import StaticGround
import Body.Terrain as Terrain
import Body.Wedge exposing (wedge)
import Primitive.Cube exposing (skyCube, fireCube, fogMountainsCube, voronoiCube, cubeMesh)
import Primitive.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Primitive.Sphere exposing (skySphere, cloudsSphere)
import Vehicles.DreamBird as DreamBird
import Vehicles.DreamBuggy as DreamBuggy
import Vehicles.Walking as Walking


main : Navigator RAM.Model RAM.Msg
main =
    RAM.create
        [ { id = "world1"
          , label = "Dreambuggy"
          , backgroundColor = rgb 135 206 235
          , apps =
                [ StaticGround.create Terrain.generate
                -- , Sky.create skySphere
                , elmLogo

                -- , Balls.create 30

                , textureCube

                , deltaWedge

                , buggy
                , aston_martin

{-
                , Obj.create
                    [ id "clouds-sphere"
                    , label "Clouds Sphere"
                    , position <| vec3 3 10 5
                    , overlay  <| Html.text "Clouds sphere"
                    , object   <| Object.Appearance cloudsSphere (vec3 2 2 2)
                    ]

                , Obj.create
                    [ id "landscape-diamond"
                    , label "Landscape Diamond"
                    , position <| vec3 40 1.5 28
                    , object   <| Object.Appearance fogMountainsDiamond (vec3 2 2 2)
                    ]

                , Obj.create
                    [ id "fire-cube"
                    , label "Fire Cube"
                    , position <| vec3 21 0 -25
                    , object   <| Object.Appearance fireCube (vec3 1 1 1)
                    , portal   <| Remote "world2" (Facing "fire-cube")
                    ]

                , Obj.create
                    [ id "sky-diamond"
                    , label "Sky Diamond"
                    , position <| vec3 -15 1.5 21
                    , object   <| Object.Appearance cloudsDiamond (vec3 2 2 2)
                    , portal   <| Remote "world2" (Behind "shufflepuck")
                    ]

                , Boids.create 100

                , Obj.create
                    [ id "voronoi-cube"
                    , label "Voronoi Cube"
                    , position <| vec3 10 0 10
                    , object   <| Object.Appearance voronoiCube (vec3 1 1 1)
                    , portal   <| Local (Become "boids")
                    ]

                , Obj.create
                    [ id "landscape-cube"
                    , label "Landscape Cube"
                    , position <| vec3 10 1.5 -10
                    , object   <| Object.Appearance fogMountainsCube (vec3 1 1 1)
                    ]
-}
                ]
          , defaultSelf = avatar 8.0
          }
        , { id = "world2"
          , label = "Shufflepuck Club"
          , backgroundColor = rgb 255 255 255
          , apps =
                [ BoxRoom.create { dimensions = vec3 20 50 30 }

                , let
                    s =
                        Shufflepuck.default
                  in
                    Shufflepuck.create
                        { s
                            | id = "shufflepuck"
                            , position = vec3 0 0 0
                        }

                , Obj.create
                    [ id "fire-cube"
                    , label "Fire Cube"
                    , position <| vec3 9 0 -14
                    , object   <| Object.Appearance fireCube (vec3 1 1 1)
                    , portal   <| Remote "world1" (Facing "fire-cube")
                    ]
                ]
          , defaultSelf = avatar 5.7

          {-
             , defaultSelf =
                 Statue.create
                     { label = "Hippy actualization",  position = vec3 30 50 6, appear = cloudsSphere }
          -}
          }
        ]

deltaWedge : ( App, Cmd AppMsg )
deltaWedge =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Delta Wedge" ]
                , Html.text "A manoueverable delta wing with solid plasma anti-gravity thrusters and zero-friction flight surfaces."
                , Html.br [] []
                , Html.hr [] []
                , DreamBird.overlay
                ]
    in
        Obj.create
            [ id "wedge"
            , label "Delta Wedge"
            , position <| vec3 23 0 12
            , overlay  <| html
            , object   <| Object.Appearance wedge (vec3 1 1 1)
            , vehicle  <|
                { drive = DreamBird.drive
                , vehicle =
                    { speed = 20.0
                    , height = 0.7
                    , radius = 1.0
                    }
                }
            ]

textureCube : ( App, Cmd AppMsg )
textureCube =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "A wooden box" ]
                , Html.text "This highly attractive wooden box doubles as a secret vehicle."
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]
    in
        Obj.create
            [ id "crate"
            , label "Wooden crate"
            , position <| vec3 -2 0 17
            , overlay html
            , object <| Object.FlatTexture
                { mesh = cubeMesh
                , texturePath = "resources/woodCrate.jpg"
                }
            , vehicle <|
                { drive = DreamBuggy.drive
                , vehicle =
                    { speed = 8.0
                    , height = 1.0
                    , radius = 1.0
                    }
                }
            ]

buggy : ( App, Cmd AppMsg )
buggy =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Buggy" ]
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]
    in
        Obj.create
            [ id "buggy"
            , label "Buggy"
            , position <| vec3 37 0 43
            , overlay  <| html
            , object   <| Object.texturedObjWith
                "OffRoad Car/Models/OFF -Road car  3D Models.obj"
                "textures/elmLogoDiffuse.png"
                "textures/elmLogoNorm.png"
                -- [ offset   <| ModelSpace 221.670662 39.350132 -132.558582
                -- [ offset   <| ModelSpace 221.670662 0 -132.558582
                [ offset   <| FloorCenter
                , scale    <| Width 6.0
                , forward  <| V3.i
                ]
            , vehicle <|
                { drive = DreamBuggy.drive
                , vehicle =
                    { speed = 16.0
                    , height = 1.2
                    , radius = 1.0
                    }
                }
            ]


aston_martin : ( App, Cmd AppMsg )
aston_martin =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Aston Martin" ]
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]
    in
        Obj.create
            [ id "aston_martin"
            , label "Aston Martin"
            , position <| vec3 -12 0 43
            , overlay  <| html
            , object   <| Object.texturedObjWith
                "aston_martin/DB9.obj"
                "textures/elmLogoDiffuse.png"
                "textures/elmLogoNorm.png"
                [ offset   <| FloorCenter
                , scale    <| Width 8.0
                , forward  <| V3.j
                ]
            , vehicle <|
                { drive = DreamBuggy.drive
                , vehicle =
                    { speed = 20.0
                    , height = 1.2
                    , radius = 1.0
                    }
                }
            ]



elmLogo : ( App, Cmd AppMsg )
elmLogo =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Elm Logo" ]
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]
    in
        Obj.create
            [ id "elm-logo"
            , label "Elm Logo"
            , position <| vec3 38 0 12
            , overlay  <| html
            , object   <| Object.texturedObj
                 "meshes/elmLogo.obj"
                 "textures/elmLogoDiffuse.png"
                 "textures/elmLogoNorm.png"
            , vehicle <|
                { drive = DreamBuggy.drive
                , vehicle =
                    { speed = 8.0
                    , height = 1.2
                    , radius = 1.0
                    }
                }
            ]


avatar : Float -> ( App, Cmd AppMsg )
avatar speed =
    let
        html =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Avatar" ]
                , Html.br [] []
                , Html.hr [] []
                , Walking.overlay
                ]
    in
        Obj.create
            [ id "avatar"
            , label "Walking"
            , position <| vec3 0 10 0
            , overlay  <| html
            , object <| Object.ReflectiveObj
                { meshPath = "meshes/suzanne.obj"
                , reflectionTexturePath = "textures/elmLogoDiffuse.png"
                }
            , vehicle <|
                { drive = Walking.drive
                , vehicle =
                    { speed = speed
                    , height = 1.4
                    , radius = 1.5
                    }
                }
            ]
