module Object.ObjUtil exposing
    ( toWorld_Mesh
    , toWorld_MeshWithVertexWithTexture
    )


import Dict exposing (Dict)
import Location exposing (Scale(..), Offset(..))
import Math.Matrix4 as M4
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Orientation exposing (Orientation)
import Object.Util exposing (..)
import OBJ.Types as Obj exposing (ObjFile, Mesh(..), MeshWith, VertexWithTexture)


-- | Load a mesh into world coordinates, with given offset, scale, rotation
toWorld_Mesh : Offset -> Scale -> Maybe Orientation -> ObjFile -> (List Obj.Mesh, Vec3)
toWorld_Mesh offset scale rotation mesh =
    let
        positions : Obj.Mesh -> List Vec3
        positions mesh =
            case mesh of
                Obj.WithoutTexture m ->
                    List.map .position m.vertices

                Obj.WithTexture m ->
                    List.map .position m.vertices

                Obj.WithTextureAndTangent m ->
                    List.map .position m.vertices


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

        transformMeshes : (Vec3 -> Vec3) -> List Mesh -> List Mesh
        transformMeshes t =
            List.map (transform t)

        meshes =
            Dict.values mesh
                |> List.concatMap Dict.values

    in
        toWorldCoords offset scale rotation (List.concatMap positions) transformMeshes meshes


toWorld_MeshWithVertexWithTexture : Offset -> Scale -> Maybe Orientation
    -> MeshWith VertexWithTexture
    -> (MeshWith VertexWithTexture, Vec3)
toWorld_MeshWithVertexWithTexture offset scale rotation mesh =
    let
        getModelCoords : MeshWith VertexWithTexture -> List Vec3
        getModelCoords m =
            List.map .position m.vertices

        transform : (Vec3 -> Vec3) -> MeshWith VertexWithTexture -> MeshWith VertexWithTexture
        transform t mesh =
            let
                t_onto vertex = { vertex | position = t vertex.position }
            in
                { mesh | vertices = List.map t_onto mesh.vertices }
    in
        toWorldCoords offset scale rotation getModelCoords transform mesh


toWorldCoords : Offset -> Scale -> Maybe Orientation
    -> (a -> List Vec3)
    -> ((Vec3 -> Vec3) -> a -> a)
    -> a
    -> (a, Vec3)
toWorldCoords offset scale rotation getModelCoords mapTransform model =
    let
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

        ( modelOrigin, modelDimensions ) =
            bounds (debugBounds (getModelCoords model))
                |> Debug.log "modelDimensions"

        modelToWorld v =
            rotate rotation v
                -- (uncenetered modelPosition, worldOrientation)
                |> rescale modelDimensions rotation scale

        -- (uncenetered worldPosition, worldOrientation)
        ( worldOrigin, worldDimensions ) =
            transformBounds modelToWorld ( modelOrigin, modelDimensions )
                |> Debug.log "(worldOrigin, worldDimensions)"

        t : Vec3 -> Vec3
        t v =
            -- (uncentered modelPosition, modelOrientation)
            modelToWorld v
                -- (uncenetered worldPosition, worldOrientation)
                -- offset (in world space)
                |> translate modelToWorld worldOrigin worldDimensions offset

        -- (centered worldPosition, worldOrientation)
        --
        newMeshes =
           (mapTransform t) model
    in
        (newMeshes, worldDimensions)


