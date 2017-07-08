module Main exposing (main)

import App exposing (App, AppMsg)
import Html exposing (Html)
import Math.Vector3 exposing (vec3)
import Model exposing (Args)
import RAM
import Boids
import Balls
import Obj
import Shufflepuck
import Sky
import Statue
import TextureCube
import StaticGround
import Suzanne
import Wedge
import Body.Terrain as Terrain
import Location exposing (..)
import Primitive.Cube exposing (skyCube, fireCube, fogMountainsCube, voronoiCube)
import Primitive.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Primitive.Sphere exposing (skySphere, cloudsSphere)
import Vehicles.DreamBuggy as DreamBuggy


main : Program Args (Model.Model RAM.Model RAM.Msg) (Model.Msg RAM.Msg)
main =
    RAM.create
        [ { id = "world1"
          , label = "Balls World"
          , apps =
                [ StaticGround.create Terrain.generate
                , Sky.create skySphere
                , elmLogo
                , Balls.create 30
                , TextureCube.create "Wooden crate" "resources/woodCrate.jpg"
                , Wedge.create "Wedge" (vec3 23 0 12)
                , Statue.create
                    { id = "clouds-sphere"
                    , label = "Clouds Sphere"
                    , position = vec3 3 10 5
                    , appear = cloudsSphere
                    }
                , Statue.create
                    { id = "landscape-diamond"
                    , label = "Landscape Diamond"
                    , position = vec3 0 1.5 0
                    , appear = fogMountainsDiamond
                    }
                , Statue.portal (Remote "world2" (Facing "fire-cube"))
                    { id = "fire-cube"
                    , label = "Fire Cube"
                    , position = vec3 21 0 -25
                    , appear = fireCube
                    }
                , Statue.portal (Remote "world2" (Behind "shufflepuck"))
                    { id = "sky-diamond"
                    , label = "Sky Diamond"
                    , position = vec3 5 1.5 1
                    , appear = cloudsDiamond
                    }
                ]
          , defaultSelf =
                Suzanne.create
                    { id = "suzanne"
                    , label = "Walking"
                    , height = 1.4
                    , speed = 8.0
                    }
          }
        , { id = "world2"
          , label = "Shufflepuck World"
          , apps =
                [ StaticGround.create Terrain.generate
                , Sky.create skySphere
                , Boids.create 100
                , Statue.portal (Local (Become "boids"))
                    { id = "voronoi-cube"
                    , label = "Voronoi Cube"
                    , position = vec3 10 0 10
                    , appear = voronoiCube
                    }
                , Statue.create
                    { id = "landscape-cube"
                    , label = "Landscape Cube"
                    , appear = fogMountainsCube
                    , position = vec3 10 1.5 -10
                    }
                , let
                    s =
                        Shufflepuck.default
                  in
                    Shufflepuck.create
                        { s
                            | id = "shufflepuck"
                            , position = vec3 53 0 18
                        }
                , Statue.portal (Local (Behind "shufflepuck"))
                    { id = "sky-diamond"
                    , label = "Sky Diamond"
                    , position = vec3 5 1.5 1
                    , appear = cloudsDiamond
                    }
                , Statue.portal (Remote "world1" (Facing "fire-cube"))
                    { id = "fire-cube"
                    , label = "Fire Cube"
                    , position = vec3 21 0 -25
                    , appear = fireCube
                    }
                ]
          , defaultSelf =
                Suzanne.create
                    { id = "suzanne"
                    , label = "Walking"
                    , height = 1.4
                    , speed = 8.0
                    }

          {-
             , defaultSelf =
                 Statue.create
                     { label = "Hippy actualization",  position = vec3 30 50 6, appear = cloudsSphere }
          -}
          }
        ]


elmLogo : ( App, Cmd AppMsg )
elmLogo =
    let
        overlay =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Elm Logo" ]
                , Html.br [] []
                , Html.hr [] []
                , DreamBuggy.overlay
                ]

        drive =
            DreamBuggy.drive { speed = 8.0 }
    in
        Obj.create
            { id = "elm-logo"
            , label = "Elm Logo"
            , overlay = overlay
            , meshPath = "meshes/elmLogo.obj"
            , diffuseTexturePath = "textures/elmLogoDiffuse.png"
            , normalTexturePath = "textures/elmLogoNorm.png"
            , drive = Just drive
            }
