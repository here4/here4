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


        translate rescale dimensions offset =
            let
                offset3 =
                    offsetToVec3 rescale dimensions offset

            in
                \v -> V3.sub v offset3


        rotate rotation =
            Maybe.withDefault identity (Maybe.map Orientation.rotateBodyV rotation)


        rescale dimensions scale =
            let
                scale3 =
                    scaleToVec3 dimensions scale
            in
                M4.transform (M4.makeScale scale3)


        loadBody r =
            case ( r.mesh, r.diffTexture, r.normTexture ) of
                ( Ok mesh, Ok diffTexture, Ok normTexture ) ->
                    let
                        meshes =
                            Dict.values mesh
                                |> List.concatMap Dict.values

                        modelDimensions =
                            bounds (List.concatMap positions meshes)

                        modelToWorld =
                            rescale modelDimensions r.scale

                        worldDimensions =
                            modelToWorld modelDimensions

                        t : Vec3 -> Vec3
                        t v =
                             modelToWorld v
                             |> translate modelToWorld worldDimensions r.offset
                             |> rotate r.rotation

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
