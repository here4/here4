module Object.Util exposing (..)

import Math.Vector3 as V3 exposing (Vec3, vec3)
import OBJ.Types as Obj
import Object.Types exposing (Scale(..), Offset(..))


debugBounds : List Vec3 -> List Vec3
debugBounds vertices =
    let
        f v ( oldMinV, oldMaxV ) =
            let
                ( minX, minY, minZ ) =
                    V3.toTuple oldMinV

                ( maxX, maxY, maxZ ) =
                    V3.toTuple oldMaxV

                ( vx, vy, vz ) =
                    V3.toTuple v

                minV =
                    V3.fromTuple ( min minX vx, min minY vy, min minZ vz )

                maxV =
                    V3.fromTuple ( max maxX vx, max maxY vy, max maxZ vz )
            in
                ( minV, maxV )

        big =
            1.0e10

        bounds =
            List.foldl f ( vec3 big big big, vec3 -big -big -big )

        calcOffset ( minV, maxV ) =
            V3.add minV (V3.scale 0.5 (V3.sub maxV minV))

        dvs vs =
            let
                tup =
                    ( Debug.log "mesh bounds:" (bounds vs), vs )

                tup2 =
                    ( Debug.log "offsets:" (calcOffset ((bounds vs))), vs )
            in
                Tuple.second tup
    in
        dvs vertices



-- | Returns (modelOrigin, modelDimensions)


bounds : List Vec3 -> ( Vec3, Vec3 )
bounds =
    let
        f v ( oldMinV, oldMaxV ) =
            let
                ( minX, minY, minZ ) =
                    V3.toTuple oldMinV

                ( maxX, maxY, maxZ ) =
                    V3.toTuple oldMaxV

                ( vx, vy, vz ) =
                    V3.toTuple v

                minV =
                    V3.fromTuple ( min minX vx, min minY vy, min minZ vz )

                maxV =
                    V3.fromTuple ( max maxX vx, max maxY vy, max maxZ vz )
            in
                ( minV, maxV )

        big =
            1.0e10

        dim ( minV, maxV ) =
            ( minV, V3.sub maxV minV )
    in
        dim << List.foldl f ( vec3 big big big, vec3 -big -big -big )


transformBounds : (Vec3 -> Vec3) -> ( Vec3, Vec3 ) -> ( Vec3, Vec3 )
transformBounds f ( origin, dimensions ) =
    let
        -- Transformed origin and dimensions
        tMinV =
            f origin

        tMaxV =
            V3.add tMinV (f dimensions)

        ( x1, y1, z1 ) =
            V3.toTuple tMinV

        ( x2, y2, z2 ) =
            V3.toTuple tMaxV
    in
        bounds
            [ vec3 x1 y1 z1
            , vec3 x1 y1 z2
            , vec3 x1 y2 z1
            , vec3 x1 y2 z2
            , vec3 x2 y1 z1
            , vec3 x2 y1 z2
            , vec3 x2 y2 z1
            , vec3 x2 y2 z2
            ]


positions : Obj.Mesh -> List Vec3
positions mesh =
    case mesh of
        Obj.WithoutTexture m ->
            List.map .position m.vertices

        Obj.WithTexture m ->
            List.map .position m.vertices

        Obj.WithTextureAndTangent m ->
            List.map .position m.vertices


scaleToVec3 : Vec3 -> Scale -> Vec3
scaleToVec3 rotatedModelDimensions scale =
    let
        ( dx, dy, dz ) =
            V3.toTuple rotatedModelDimensions
    in
        case scale of
            Scale3 fx fy fz ->
                vec3 fx fy fz

            Scale f ->
                vec3 f f f

            Dimensions targetX targetY targetZ ->
                vec3 (targetX / dx) (targetY / dy) (targetZ / dz)

            Width w ->
                let
                    f =
                        w / dx
                in
                    vec3 f f f

            Height h ->
                let
                    f =
                        h / dy
                in
                    vec3 f f f

            Length l ->
                let
                    f =
                        l / dz
                in
                    vec3 f f f


offsetToVec3 :
    (Vec3 -> Vec3)
    -> Vec3
    -> Offset
    -> Vec3
offsetToVec3 modelToWorld worldDimensions offset =
    let
        ( dx, dy, dz ) =
            V3.toTuple worldDimensions
    in
        case offset of
            ModelSpace mx my mz ->
                modelToWorld (vec3 mx my mz)

            WorldSpace wx wy wz ->
                vec3 wx wy wz

            Center ->
                vec3 (dx / 2) (dy / 2) (dz / 2)

            FloorCenter ->
                vec3 (dx / 2) 0 (dz / 2)
