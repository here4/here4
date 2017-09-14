module Geometry.VertexStrip exposing (..)

import List exposing (drop, map3)
import Math.Vector3 as V3 exposing (Vec3)


type alias VertexLike v =
    { v
        | position : Vec3
        , normal : Vec3
    }


-- Make the next pair of vertices for a strip, with successive normals
vertexPair : VertexLike v -> VertexLike v -> VertexLike v -> VertexLike v -> List (VertexLike v)
vertexPair v1 v2 v3 v4 =
    let
        mkVertex u1 u2 u3 =
            let
                p1 =
                    u1.position

                p2 =
                    u2.position

                p3 =
                    u3.position
            in
                { u1 | normal = V3.cross (V3.sub p2 p1) (V3.sub p3 p1) }
    in
        [ mkVertex v1 v2 v3, mkVertex v2 v3 v4 ]


mkStrip : List (VertexLike v) -> List (VertexLike v) -> List (VertexLike v)
mkStrip vs1 vs2 =
    case ( vs1, vs2 ) of
        ( [ v10, v11 ], [ v20, v21 ] ) ->
            vertexPair v10 v20 v11 v21 ++ vertexPair v11 v21 v10 v20

        ( v10 :: v11 :: r1, v20 :: v21 :: r2 ) ->
            vertexPair v10 v20 v11 v21 ++ mkStrip (v11 :: r1) (v21 :: r2)

        _ ->
            []


-- Make a triangle facet, with normals at all vertices pointing in the
-- same direction


makeFacet :
    VertexLike v
    -> VertexLike v
    -> VertexLike v
    -> ( VertexLike v, VertexLike v, VertexLike v )
makeFacet v1 v2 v3 =
    let
        p1 =
            v1.position

        p2 =
            v2.position

        p3 =
            v3.position

        normal =
            V3.cross (V3.sub p2 p1) (V3.sub p3 p1)

        setNormal v =
            { v | normal = normal }
    in
        ( setNormal v1, setNormal v2, setNormal v3 )


mkStripMaybe :
    List (Maybe (VertexLike v))
    -> List (Maybe (VertexLike v))
    -> List ( VertexLike v, VertexLike v, VertexLike v )
mkStripMaybe vs1 vs2 =
    let
        mkMaybe triangle =
            case triangle of
                ( Just v1, Just v2, Just v3 ) ->
                    Just (makeFacet v1 v2 v3)

                _ ->
                    Nothing

        strip =
            map3 (,,) vs1 vs2 (drop 1 vs1) ++ map3 (,,) vs2 (drop 1 vs1) (drop 1 vs2)
    in
        List.filterMap mkMaybe strip

