module Main exposing (main)

import Html exposing (Html)
import Location exposing (..)
import Math.Vector3 exposing (vec3)
import Navigator exposing (..)
import RAM

import Boids
import BoxRoom
import Balls
import Obj
import Object
import Shufflepuck
import Sky
import Statue
import TextureCube
import StaticGround
import Wedge
import Body.Terrain as Terrain
import Primitive.Cube exposing (skyCube, fireCube, fogMountainsCube, voronoiCube)
import Primitive.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Primitive.Sphere exposing (skySphere, cloudsSphere)
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
                    , position = vec3 40 1.5 28
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
                    , position = vec3 -15 1.5 21
                    , appear = cloudsDiamond
                    }

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
                ]
          , defaultSelf = avatar 16.0
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

                , Statue.portal (Remote "world1" (Facing "fire-cube"))
                    { id = "fire-cube"
                    , label = "Fire Cube"
                    , position = vec3 9 0 -14
                    , appear = fireCube
                    }
                ]
          , defaultSelf = avatar 5.7

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
    in
        Obj.create
            { id = "elm-logo"
            , label = "Elm Logo"
            , position = vec3 38 0 12
            , overlay = overlay
            , object = Object.TexturedObj
                { meshPath = "meshes/elmLogo.obj"
                , diffuseTexturePath = "textures/elmLogoDiffuse.png"
                , normalTexturePath = "textures/elmLogoNorm.png"
                }
            , drive = Just DreamBuggy.drive
            , vehicle =
                { speed = 8.0
                , height = 1.2
                , radius = 1.0
                }
            }


avatar : Float -> ( App, Cmd AppMsg )
avatar speed =
    let
        overlay =
            Html.div []
                [ Html.h2 []
                    [ Html.text "Avatar" ]
                , Html.br [] []
                , Html.hr [] []
                , Walking.overlay
                ]
    in
        Obj.create
            { id = "avatar"
            , label = "Walking"
            , position = vec3 0 10 0
            , overlay = overlay
            , object = Object.ReflectiveObj
                { meshPath = "meshes/suzanne.obj"
                , reflectionTexturePath = "textures/elmLogoDiffuse.png"
                }
            , drive = Just Walking.drive
            , vehicle =
                { speed = speed
                , height = 1.4
                , radius = 1.5
                }
            }
