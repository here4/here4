module Object.FlatTexture
    exposing
        ( FlatTextureAttributes
        , FlatTextureResult
        , FlatTextureMsg(..)
        , flatTextureInit
        , flatTextureUpdate
        )

import Body.Obj exposing (reflective)
import Here4.Appearance exposing (Appearance)
import Dict exposing (Dict)
import Math.Vector3 as V3 exposing (vec3)
import Math.Matrix4 as M4
import Object.Types exposing (Load(..))
import Object.Util exposing (..)
import OBJ
import OBJ.Types exposing (MeshWith, VertexWithTexture)
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)
import Task exposing (Task)
import WebGL exposing (entity, Mesh)
import WebGL.Texture as Texture exposing (Texture, Error)


type alias FlatTextureAttributes =
    { mesh : Mesh Vertex
    , texturePath : String
    }


type alias FlatTextureResult =
    { mesh : Mesh Vertex
    , texture : Result String Texture
    }


type FlatTextureMsg
    = TextureLoaded (Result String Texture)


flatTextureInit : FlatTextureAttributes -> ( Load FlatTextureResult, Cmd FlatTextureMsg )
flatTextureInit attributes =
    ( Loading
        { mesh = attributes.mesh
        , texture = Err "Loading texture ..."
        }
      -- , Texture.load attributes.texturePath
      --     |> Task.attempt TextureLoaded
    , loadTexture attributes.texturePath TextureLoaded
    )


flatTextureUpdate : FlatTextureMsg -> Load FlatTextureResult -> ( Load FlatTextureResult, Cmd FlatTextureMsg )
flatTextureUpdate msg model =
    let
        loadBody m =
            case m.texture of
                Ok texture_ ->
                    Ready (makeAppearance m.mesh texture_) (vec3 1 1 1)

                _ ->
                    Loading m
    in
        case model of
            Ready appear dimensions ->
                ( Ready appear dimensions, Cmd.none )

            Loading partial ->
                case msg of
                    TextureLoaded textureResult ->
                        ( loadBody { partial | texture = textureResult }, Cmd.none )


loadTexture : String -> (Result String Texture -> msg) -> Cmd msg
loadTexture url msg =
    Texture.load url
        |> Task.attempt
            (\r ->
                case r of
                    Ok t ->
                        msg (Ok t)

                    Err e ->
                        msg (Err ("Failed to load texture: " ++ toString e))
            )


makeAppearance : Mesh Vertex -> Texture -> Appearance
makeAppearance mesh texture p =
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
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , modelMatrix = M4.identity
            , viewPosition = p.cameraPos
            , lightPosition = p.lightPosition
            , ambientColor = p.ambientColor
            }
        ]
