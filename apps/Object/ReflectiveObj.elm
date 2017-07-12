module Object.ReflectiveObj exposing
    ( ReflectiveObjAttributes
    , ReflectiveObjResult
    , ReflectiveObjMsg(..)
    , reflectiveObjInit
    , reflectiveObjUpdate
    )


import Appearance exposing (Appearance)
import Body.Obj exposing (reflective)
import Dict exposing (Dict)
import Object.Types exposing (Load(..))
import OBJ
--import OBJ.Types exposing (ObjFile, Mesh(..))
import OBJ.Types exposing (MeshWith, VertexWithTexture)
import Task exposing (Task)
import WebGL.Texture as Texture exposing (Texture, Error)


type alias ReflectiveObjAttributes =
    { meshPath : String
    , diffuseTexturePath : String
    }

type alias ReflectiveObjResult =
    { mesh : Result String (MeshWith VertexWithTexture)
    , reflectionTexture : Result String Texture
    }

type ReflectiveObjMsg
    = DiffTextureLoaded (Result String Texture)
    | LoadObj (Result String (MeshWith VertexWithTexture))


reflectiveObjInit : ReflectiveObjAttributes -> (Load ReflectiveObjResult, Cmd ReflectiveObjMsg)
reflectiveObjInit attributes =
    ( Loading
          { mesh = Err "Loading ..."
          , reflectionTexture = Err "Loading texture ..."
          }
    , Cmd.batch
        [ loadTexture attributes.diffuseTexturePath DiffTextureLoaded
        , OBJ.loadMesh attributes.meshPath LoadObj
        ]
    )


reflectiveObjUpdate : ReflectiveObjMsg -> Load ReflectiveObjResult -> (Load ReflectiveObjResult, Cmd ReflectiveObjMsg)
reflectiveObjUpdate msg model =
    let
        loadBody m =
            case ( m.mesh, m.reflectionTexture ) of
                ( Ok mesh, Ok texture ) ->
                    Ready (reflective mesh texture)
                _ ->
                    Loading m
    in
        case model of
            Ready appear ->
                ( Ready appear, Cmd.none )

            Loading partial ->
            
                case msg of
                    DiffTextureLoaded textureResult ->
                        ( loadBody { partial | reflectionTexture = textureResult }, Cmd.none )
                    LoadObj meshResult ->
                        ( loadBody { partial | mesh = meshResult }, Cmd.none )


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

