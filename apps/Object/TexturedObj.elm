module Object.TexturedObj exposing
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
import Math.Matrix4 as M4
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Object.Types exposing (..)
import Object.Util exposing (..)
import OBJ
import OBJ.Types as Obj exposing (ObjFile, Mesh(..))
import Orientation exposing (Orientation)
import Task exposing (Task)
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
    { mesh : Result String ObjFile
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

texturedObjInit : TexturedObjAttributes -> (Load TexturedObjResult, Cmd TexturedObjMsg)
texturedObjInit attributes =
    ( Loading
          { mesh = Err "Loading ..."
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


texturedObjUpdate : TexturedObjMsg -> Load TexturedObjResult -> (Load TexturedObjResult, Cmd TexturedObjMsg)
texturedObjUpdate msg model =
    let
        transform : (Vec3 -> Vec3) -> Mesh -> Mesh
        transform t mesh =
            let
                mapTransform =
                    List.map (\v -> { v | position = t v.position })

                updateVertices m =
                    { m | vertices = mapTransform m.vertices }
            in
                case mesh of
                    Obj.WithoutTexture m ->
                        Obj.WithoutTexture (updateVertices m)

                    Obj.WithTexture m ->
                        Obj.WithTexture (updateVertices m)

                    Obj.WithTextureAndTangent m ->
                        Obj.WithTextureAndTangent (updateVertices m)


        translate : (Vec3 -> Vec3) -> Vec3 -> Vec3 -> Offset -> Vec3 -> Vec3
        translate modelToWOrld worldOrigin worldDimensions offset =
            let
                offset3 =
                    offsetToVec3 modelToWOrld worldDimensions offset
            in
                \v -> V3.sub v (Debug.log "summed offset" (V3.add worldOrigin offset3))


        rotate : Maybe Orientation -> Vec3 -> Vec3
        rotate rotation =
            Maybe.withDefault identity (Maybe.map Orientation.rotateBodyV rotation)


        rescale : Vec3 -> Maybe Orientation -> Scale -> Vec3 -> Vec3
        rescale dimensions rotation scale =
            let
                scale3 =
                    scaleToVec3 (rotate rotation dimensions) scale
            in
                M4.transform (M4.makeScale scale3)


        loadBody r =
            case ( r.mesh, r.diffTexture, r.normTexture ) of
                ( Ok mesh, Ok diffTexture, Ok normTexture ) ->
                    let
                        meshes =
                            Dict.values mesh
                                |> List.concatMap Dict.values

                        (modelOrigin, modelDimensions) =
                            bounds (debugBounds (List.concatMap positions meshes))
                            |> Debug.log "modelDimensions"


                        modelToWorld v =
                            rotate r.rotation v
                            -- (uncenetered modelPosition, worldOrientation)
                            |> rescale modelDimensions r.rotation r.scale
                            -- (uncenetered worldPosition, worldOrientation)

                        (worldOrigin, worldDimensions) =
                            transformBounds modelToWorld (modelOrigin, modelDimensions)
                            |> Debug.log "(worldOrigin, worldDimensions)"

                        t : Vec3 -> Vec3
                        t v =
                            -- (uncentered modelPosition, modelOrientation)
                            modelToWorld v
                            -- (uncenetered worldPosition, worldOrientation)
                            -- offset (in world space)
                            |> translate modelToWorld worldOrigin worldDimensions r.offset
                            -- (centered worldPosition, worldOrientation)

                        newMeshes =
                            List.map (transform t) meshes

                        appearMesh =
                            textured diffTexture normTexture

                        appear p =
                            List.concatMap (\m -> appearMesh m p) newMeshes
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
                        ( loadBody { partial | diffTexture = textureResult }, Cmd.none )
                    NormTextureLoaded textureResult ->
                        ( loadBody { partial | normTexture = textureResult }, Cmd.none )
                    LoadObj url meshResult ->
                        ( loadBody { partial | mesh = meshResult }, Cmd.none )


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

