module Object.TexturedObj exposing
    ( TexturedObjAttributes
    , TexturedObjResult
    , TexturedObjMsg(..)
    , texturedObjInit
    , texturedObjUpdate
    )


import Appearance exposing (Appearance)
import Body.Obj exposing (textured)
import Dict exposing (Dict)
import Math.Vector3 as V3 exposing (Vec3)
import Object.Types exposing (Load(..))
import OBJ
import OBJ.Types as Obj exposing (ObjFile, Mesh(..))
import Task exposing (Task)
import WebGL.Texture as Texture exposing (Texture, Error)


type alias TexturedObjAttributes =
    { meshPath : String
    , diffuseTexturePath : String
    , normalTexturePath : String
    , offset : Vec3
    }

type alias TexturedObjResult =
    { mesh : Result String ObjFile
    , diffTexture : Result String Texture
    , normTexture : Result String Texture
    , offset : Vec3
    }

type TexturedObjMsg
    = DiffTextureLoaded (Result String Texture)
    | NormTextureLoaded (Result String Texture)
    | LoadObj String (Result String (Dict String (Dict String Mesh)))


texturedObjInit : TexturedObjAttributes -> (Load TexturedObjResult, Cmd TexturedObjMsg)
texturedObjInit attributes =
    ( Loading
          { mesh = Err "Loading ..."
          , diffTexture = Err "Loading texture ..."
          , normTexture = Err "Loading texture ..."
          , offset = attributes.offset
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
{-
        debugBounds vertices =
            let
                f v (oldMinV, oldMaxV) =
                    let
                        (minX, minY, minZ) = V3.toTuple oldMinV
                        (maxX, maxY, maxZ) = V3.toTuple oldMaxV
                        (vx, vy, vz) = V3.toTuple v.position

                        minV = V3.fromTuple (min minX vx, min minY vy, min minZ vz)
                        maxV = V3.fromTuple (max maxX vx, max maxY vy, max maxZ vz)
                    in
                        (minV, maxV)

                big = 1e10

                bounds =
                    List.foldl f (vec3 big big big, vec3 -big -big -big)

                calcOffset (minV, maxV) =
                    add minV (V3.scale 0.5 (sub maxV minV))

                dvs vs =
                    let
                        tup =
                            (Debug.log "mesh bounds:" (bounds vs), vs)

                        tup2 =
                            (Debug.log "offsets:" (calcOffset ((bounds vs))), vs)
                    in
                        Tuple.second tup

            in
                dvs vertices
-}

        applyOffset offset mesh =
            let
                translate =
                    -- debugBounds >>
                    List.map (\v -> { v | position = V3.sub v.position offset })

                updateVertices m =
                    { m | vertices = translate m.vertices }
            in
                case mesh of
                    Obj.WithoutTexture m ->
                        Obj.WithoutTexture (updateVertices m)

                    Obj.WithTexture m ->
                        Obj.WithTexture (updateVertices m)

                    Obj.WithTextureAndTangent m ->
                        Obj.WithTextureAndTangent (updateVertices m)

        loadBody r =
            case ( r.mesh, r.diffTexture, r.normTexture ) of
                ( Ok mesh, Ok diffTexture, Ok normTexture ) ->
                    let
                        meshes =
                            Dict.values mesh
                                |> List.concatMap Dict.values
                                |> List.map (applyOffset r.offset)

                        appearMesh = textured diffTexture normTexture

                        appear p =
                                List.concatMap (\m -> appearMesh m p) meshes
                    in
                        Ready appear
                _ ->
                    Loading r
    in
        case model of
            Ready appear ->
                ( Ready appear, Cmd.none )

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

