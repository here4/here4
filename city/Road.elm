module Road exposing (..)

import Geometry.VertexStrip exposing (..)
import Here4.App as App exposing (..)
import Here4.App.Types exposing (..)
import Here4.Appearance exposing (..)
import Here4.Body exposing (..)
import Here4.Orientation as Orientation
import Html exposing (Html)
import Html.Attributes as Html
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (vec4)
import Math.Matrix4 as M4
import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)
import WebGL exposing (Entity, Mesh, entity, triangleStrip)


type alias Model =
    { path : List Vec3
    , bodies : List Body
    }

type alias Msg =
    ()


create : List Vec3 -> Vec3 -> ( App, Cmd AppMsg )
create path startPos =
    App.create (init path startPos)
        { id = always "road"
        , label = always "Road"
        , update = update
        , animate = animate
        , bodies = bodies
        , framing = noFraming
        , focus = always Nothing
        , overlay = overlay
        , reposition = always identity
        }


init : List Vec3 -> Vec3 -> ( Model, Cmd (CtrlMsg Msg) )
init path startPos =
    let
        body = generateRoad 5.0 path startPos
    in
        ( { path = path
          , bodies = [body]
          }
        , Cmd.none
        )


update : CtrlMsg Msg -> Model -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    ( model, Cmd.none )


animate : Ground -> Time -> Model -> ( Model, Cmd (CtrlMsg Msg) )
animate ground dt model =
    ( model, Cmd.none )


bodies : Model -> Vec3 -> List Body
bodies model pos =
    model.bodies


overlay : Model -> Html msg
overlay _ =
    Html.text "Road"


----------------------------------------------------------------------


type alias RoadVertex =
    { position : Vec3
    , normal : Vec3
    , coord : Vec3
    }


generateRoad : Float -> List Vec3 -> Vec3 -> Body
generateRoad sideWidth path startPos =
    let
        appear =
            roadAppearance sideWidth path
    in
        { anchor = AnchorGround
        , scale = vec3 1 1 1
        , position = startPos
        , orientation = Orientation.initial
        , appear = appear
        }


roadAppearance : Float -> List Vec3 -> Perception -> List Entity
roadAppearance sideWidth path p =
    let
        resolution =
            vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0

        s =
            p.globalTime

        detail =
            p.measuredFPS / 3.0

        iHMD =
            if p.cameraVR then
                1.0
            else
                0.0

        mesh =
            roadMesh sideWidth path
    in
        [ entity noiseVertex
            noiseColorFragment
            mesh
            { iResolution = resolution
            , iHMD = iHMD
            , iDetail = detail
            , iGlobalTime = s
            , iGlobalTimeV = s
            , iLensDistort = p.lensDistort
            , modelViewProjectionMatrix = M4.mul p.perspective p.lookAt
            , modelMatrix = M4.identity
            , viewPosition = p.cameraPos
            , lightPosition = p.lightPosition
            , ambientColor = p.ambientColor
            }
        ]


roadMesh : Float -> List Vec3 -> Mesh NoiseVertex
roadMesh sideWidth path =
    let
        toNoiseVertex v =
            { position = v.position
            , normal = v.normal
            , coord = v.coord
            , color = vec4 0.4 0.4 0.4 0.5
            , smoothing = 0.1
            , textureScale = 1.0
            , timeScale = 0.0
            }

        (leftSide, rightSide) =
            roadSides sideWidth (toRoadVertices path)
    in
        mkStrip
            (List.map toNoiseVertex leftSide)
            (List.map toNoiseVertex rightSide)
        |> triangleStrip
    

toRoadVertices : List Vec3 -> List RoadVertex
toRoadVertices path =
    let
        start v1 v2 =
            { position = v1
            , normal = uprightNormal v1 v2
            , coord = vec3 0 0 0
            }
        end v1 v2 prev =
            { position = v2
            , normal = uprightNormal v1 v2
            , coord = nextCoord v1 v2 prev.coord
            }
        middle v1 v2 v3 prev =
            { position = v2
            , normal = interpolateNormal v1 v2 v3
            , coord = nextCoord v1 v2 prev.coord
            }
    in
        foldTriple start end middle path


nextCoord : Vec3 -> Vec3 -> Vec3 -> Vec3
nextCoord v1 v2 prevCoord =
    let
        d = V3.distance v1 v2
        y = V3.getY prevCoord
    in
        V3.setY (y+d) prevCoord

uprightNormal : Vec3 -> Vec3 -> Vec3
uprightNormal v1 v2 =
    let
        rise = V3.normalize (V3.sub v2 v1)
        crossWalk = V3.cross V3.j rise
    in
        V3.cross crossWalk rise


interpolateNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
interpolateNormal v1 v2 v3 =
    let
        norm12 = uprightNormal v1 v2
        norm23 = uprightNormal v2 v3
    in
        V3.add norm12 norm23
        |> V3.scale 0.5


roadSides : Float -> List RoadVertex -> (List RoadVertex, List RoadVertex)
roadSides roadWidth path =
    (side (-roadWidth/2) path, side (roadWidth/2) path)


side : Float -> List RoadVertex -> List RoadVertex
side sideWidth path =
    let
        mapX : (Float -> Float) -> Vec3 -> Vec3
        mapX f v =
            V3.setX (f (V3.getX v)) v

        start v1 v2 =
            { v1 | position = V3.add v1.position (sideOffset sideWidth v1.position v2.position)
                 , coord = mapX (\x -> x + sideWidth) v1.coord
            }
        end v1 v2 =
            { v2 | position = V3.add v2.position (sideOffset sideWidth v1.position v2.position)
                 , coord = mapX (\x -> x + sideWidth) v2.coord
            }
        middle v1 v2 v3 =
            { v2 | position = corner sideWidth v1.position v2.position v3.position
                 , coord = mapX (\x -> x + sideWidth) v2.coord
            }
    in
        mapTriple start end middle path


-- Given a path segment (v1, v2), return the offset of the roadside
-- ie. if the center of the road is the line v1->v2, then the
-- roadside passes through the line (v1+offset)->(v2+offset)
-- This assumes no banking, ie. the offset is always in the XZ plane
sideOffset : Float -> Vec3 -> Vec3 -> Vec3
sideOffset sideWidth v1 v2 =
    let
        segmentUnit =
            V3.normalize (V3.sub v2 v1)

        segmentNorm =
            vec3 (V3.getZ segmentUnit) 0 (V3.getX segmentUnit)
    in
        V3.scale sideWidth segmentNorm


-- Position of the corner of the roadside
-- Given a path (v1, v2, v3), return the position of the corner
-- near v2
corner : Float -> Vec3 -> Vec3 -> Vec3 -> Vec3
corner sideWidth v1 v2 v3 =
    let
        offset12 = sideOffset sideWidth v1 v2
        offset23 = sideOffset sideWidth v2 v3

        -- Just find where the second line intersects the first plane

        p1 = V3.add v1 offset12
        p2 = V3.add v2 offset23

        norm u v w =
            V3.normalize <| V3.cross (V3.sub u v) (V3.sub w v)

        norm12 = norm p1 v1 v2
        norm23 = norm p2 v2 v3

        -- V3.cross norm12 norm23

        getXZ v = (V3.getX v, V3.getZ v)

        -- The two roadsides, in (x,y) notation
        (x1, y1) = getXZ <| V3.add v1 offset12
        (x2, y2) = getXZ <| V3.add v2 offset12
        (x3, y3) = getXZ <| V3.add v2 offset23
        (x4, y4) = getXZ <| V3.add v3 offset23

        denominator = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)

        x = (x1*y2 - y1*x2)*(x3-x4) - (x1-x2)*(x3*y4 - y3*x4)
        y = (x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x3*y4 - y3*x4)
    in
        if abs denominator < 0.0001 then
            V3.add v2 offset12
        else
            vec3 (x/denominator) 0 (y/denominator)
            



----------------------------------------------------------------------

mapPair : (a -> b) -> (a -> a -> b) -> List a -> List b
mapPair ending f xs =
    case xs of
        ( x1 :: x2 :: rest ) ->
            f x1 x2 :: mapPair ending f (x2 :: rest)
        [x] ->
            [ending x]
        [] ->
            []


mapTriple : (a -> a -> b) -> (a -> a -> b) -> (a -> a -> a -> b) -> List a -> List b
mapTriple start end middle xs0 =
    let
        f xs =
            case xs of
                ( x1 :: x2 :: x3 :: rest ) ->
                    middle x1 x2 x3 :: f (x2 :: x3 :: rest)
                [ x1, x2 ] ->
                    [end x1 x2]
                _ ->
                    []
    in
        case xs0 of
            ( x1 :: x2 :: rest ) ->
                start x1 x2 :: f xs0
            _ ->
                []

foldTriple : (a -> a -> b) -> (a -> a -> b -> b) -> (a -> a -> a -> b -> b) -> List a -> List b
foldTriple start end middle xs0 =
    let
        f b0 xs =
            case xs of
                ( x1 :: x2 :: x3 :: rest ) ->
                    let
                        b = middle x1 x2 x3 b0
                    in
                        b :: f b (x2 :: x3 :: rest)
                [ x1, x2 ] ->
                    [end x1 x2 b0]
                _ ->
                    []
    in
        case xs0 of
            ( x1 :: x2 :: rest ) ->
                let
                    init = start x1 x2
                in
                    init :: f init xs0
            _ ->
                []
