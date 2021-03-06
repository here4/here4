module Here4.Object.ReflectiveObj
    exposing
        ( ReflectiveObjAttributes
        , ReflectiveObjResult
        , ReflectiveObjMsg(..)
        , reflectiveObj
        , reflectiveObjInit
        , reflectiveObjUpdate
        )

import Dict exposing (Dict)
import Here4.Appearance exposing (Appearance)
import Here4.Body.Obj exposing (reflective)
import Here4.Location exposing (..)
import Here4.Orientation exposing (Orientation)
import Here4.Object.ObjUtil exposing (toWorld_MeshWithVertexWithTexture)
import Here4.Object.Types exposing (Load(..))
import Here4.Object.Util exposing (..)
import Math.Vector3 as V3 exposing (vec3)
import OBJ
import OBJ.Types exposing (MeshWith, VertexWithTexture)
import Task exposing (Task)
import WebGL exposing (indexedTriangles)
import WebGL.Texture as Texture exposing (Texture, Error)


type alias ReflectiveObjAttributes =
    { meshPath : String
    , reflectionTexturePath : String
    , offset : Offset
    , scale : Scale
    , rotation : Maybe Orientation
    }


type alias ReflectiveObjResult =
    { mesh : Result String (MeshWith VertexWithTexture)
    , reflectionTexture : Result String Texture
    , offset : Offset
    , scale : Scale
    , rotation : Maybe Orientation
    }


type ReflectiveObjMsg
    = DiffTextureLoaded (Result String Texture)
    | LoadObj (Result String (MeshWith VertexWithTexture))


reflectiveObj : String -> String -> ReflectiveObjAttributes
reflectiveObj meshPath reflectionTexturePath =
    { meshPath = meshPath
    , reflectionTexturePath = reflectionTexturePath
    , offset = WorldSpace 0 0 0
    , scale = Scale 1.0
    , rotation = Nothing
    }


reflectiveObjInit : ReflectiveObjAttributes -> ( Load ReflectiveObjResult, Cmd ReflectiveObjMsg )
reflectiveObjInit attributes =
    ( Loading
        { mesh = Err "Loading ..."
        , reflectionTexture = Err "Loading texture ..."
        , offset = attributes.offset
        , scale = attributes.scale
        , rotation = attributes.rotation
        }
    , Cmd.batch
        [ loadTexture attributes.reflectionTexturePath DiffTextureLoaded
        , OBJ.loadMesh attributes.meshPath LoadObj
        ]
    )


reflectiveObjUpdate : ReflectiveObjMsg -> Load ReflectiveObjResult -> ( Load ReflectiveObjResult, Cmd ReflectiveObjMsg )
reflectiveObjUpdate msg model =
    let
        loadBody r =
            case ( r.mesh, r.reflectionTexture ) of
                ( Ok mesh, Ok texture ) ->
                    let
                        ( { vertices, indices }, worldDimensions ) =
                            toWorld_MeshWithVertexWithTexture r.offset r.scale r.rotation mesh

                        newMesh =
                            indexedTriangles vertices indices

                        appear p =
                            reflective newMesh texture p
                    in
                        Ready appear worldDimensions

                _ ->
                    Loading r
    in
        case model of
            Ready appear dimensions ->
                ( Ready appear dimensions, Cmd.none )

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
