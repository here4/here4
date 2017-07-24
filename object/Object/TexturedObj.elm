module Object.TexturedObj
    exposing
        ( TexturedObjAttributes
        , TexturedObjResult
        , TexturedObjMsg(..)
        , texturedObj
        , texturedObjInit
        , texturedObjUpdate
        )

import Appearance exposing (Appearance)
import Body.Obj exposing (textured)
import Dict exposing (Dict)
import Location exposing (Offset(..), Scale(..))
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Object.ObjUtil exposing (toWorld_Mesh)
import Object.Types exposing (..)
import OBJ
import OBJ.Types as Obj exposing (ObjFile, Mesh(..))
import Orientation exposing (Orientation)
import Shaders.Obj as Shaders
import Task exposing (Task)
import WebGL exposing (indexedTriangles)
import WebGL.Texture as Texture exposing (Texture, Error)
import Debug


type alias TexturedObjAttributes =
    { meshPath : String
    , diffuseTexturePath : String
    , normalTexturePath : String
    , offset : Offset
    , scale : Scale
    , rotation : Maybe Orientation
    }


type alias TexturedObjResult =
    { meshes : List (Texture -> Texture -> Appearance)
    , worldDimensions : Maybe Vec3
    , diffTexture : Result String Texture
    , normTexture : Result String Texture
    , offset : Offset
    , scale : Scale
    , rotation : Maybe Orientation
    }


type TexturedObjMsg
    = DiffTextureLoaded (Result String Texture)
    | NormTextureLoaded (Result String Texture)
    | LoadObj String (Result String (Dict String (Dict String Mesh)))


texturedObj : String -> String -> String -> TexturedObjAttributes
texturedObj meshPath diffuseTexturePath normalTexturePath =
    { meshPath = meshPath
    , diffuseTexturePath = diffuseTexturePath
    , normalTexturePath = normalTexturePath
    , offset = WorldSpace 0 0 0
    , scale = Scale 1.0
    , rotation = Nothing
    }


texturedObjInit : TexturedObjAttributes -> ( Load TexturedObjResult, Cmd TexturedObjMsg )
texturedObjInit attributes =
    ( Loading
        { meshes = []
        , worldDimensions = Nothing
        , diffTexture = Err "Loading texture ..."
        , normTexture = Err "Loading texture ..."
        , offset = attributes.offset
        , scale = attributes.scale
        , rotation = attributes.rotation
        }
    , Cmd.batch
        [ loadTexture attributes.diffuseTexturePath DiffTextureLoaded
        , loadTexture attributes.normalTexturePath NormTextureLoaded
        , loadModel True attributes.meshPath
        ]
    )


texturedObjUpdate : TexturedObjMsg -> Load TexturedObjResult -> ( Load TexturedObjResult, Cmd TexturedObjMsg )
texturedObjUpdate msg model =
    let
        loadBody r =
            case ( r.worldDimensions, r.diffTexture, r.normTexture ) of
                ( Just worldDimensions, Ok diffTexture, Ok normTexture ) ->
                    let
                        appear p =
                            List.concatMap (\f -> f diffTexture normTexture p) r.meshes
                    in
                        Ready appear worldDimensions

                _ ->
                    Loading r

        loadMeshes meshResult r =
            case meshResult of
                Ok mesh ->
                    let
                        ( newMeshes, worldDimensions ) =
                            toWorld_Mesh r.offset r.scale r.rotation mesh

                        makeAppearance mesh =
                            case mesh of
                                Obj.WithoutTexture { vertices, indices } ->
                                    textured (indexedTriangles vertices indices)
                                        Shaders.simpleVert
                                        Shaders.simpleFrag

                                Obj.WithTexture { vertices, indices } ->
                                    textured (indexedTriangles vertices indices)
                                        Shaders.noNormalVert
                                        Shaders.noNormalFrag

                                Obj.WithTextureAndTangent { vertices, indices } ->
                                    textured (indexedTriangles vertices indices)
                                        Shaders.normalVert
                                        Shaders.normalFrag
                    in
                        { r
                            | meshes = List.map makeAppearance newMeshes
                            , worldDimensions = Just worldDimensions
                        }

                _ ->
                    r
    in
        case model of
            Ready appear dimensions ->
                ( Ready appear dimensions, Cmd.none )

            Loading partial ->
                case msg of
                    DiffTextureLoaded textureResult ->
                        ( loadBody { partial | diffTexture = textureResult }, Cmd.none )

                    NormTextureLoaded textureResult ->
                        ( loadBody { partial | normTexture = textureResult }, Cmd.none )

                    LoadObj url meshResult ->
                        ( loadBody (loadMeshes meshResult partial), Cmd.none )


loadModel : Bool -> String -> Cmd TexturedObjMsg
loadModel withTangents url =
    OBJ.loadObjFileWith { withTangents = withTangents } url (LoadObj url)


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
