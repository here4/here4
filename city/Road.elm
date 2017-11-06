module Road exposing (..)

import Geometry.Projection exposing (..)
import Geometry.VertexStrip exposing (..)
import Here4.App as App exposing (..)
import Here4.App.Types exposing (..)
import Here4.Appearance exposing (..)
import Here4.Barrier exposing (GroundSurface(..), Barrier, Quad(..), barrierFromQuads, relativeBarrier)
import Here4.Body exposing (..)
import Here4.Dispatch exposing (..)
import Here4.Orientation as Orientation
import Html exposing (Html)
import Html.Attributes as Html
import List.Extra as List
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (vec4)
import Math.Matrix4 as M4
import Maybe.Extra as Maybe
import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)
import Task exposing (Task)
import WebGL exposing (Entity, Mesh, entity, triangleStrip)


type alias BankingPoint =
    { position : Vec3
    , banking : Float -- degrees
    }


toBankingPoint : Vec3 -> BankingPoint
toBankingPoint v =
    { position = v
    , banking = 0
    }


type alias Model =
    { path : List BankingPoint
    , sideWidth : Float
    , startY : Float
    , leftSide : List RoadVertex
    , rightSide : List RoadVertex
    , body : Body
    , haveSetBarrier : Bool
    }


type alias Msg =
    ()


methods =
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



-- Create a flat road (possibly hilly, with ramps and turns, but no banking)


create : Float -> List Vec3 -> Vec3 -> ( App, Cmd AppMsg )
create sideWidth path startPos =
    App.create (init sideWidth (List.map toBankingPoint path) startPos)
        methods



-- Create a road with banking


racetrack : Float -> List BankingPoint -> Vec3 -> ( App, Cmd AppMsg )
racetrack sideWidth path startPos =
    App.create (init sideWidth path startPos)
        methods


init : Float -> List BankingPoint -> Vec3 -> ( Model, Cmd (CtrlMsg Msg) )
init sideWidth path startPos =
    let
        ( leftSide, rightSide ) =
            roadSides sideWidth (toRoadVertices path)

        body =
            generateRoad leftSide rightSide startPos

        model =
            { path = path
            , sideWidth = sideWidth
            , startY = V3.getY startPos
            , leftSide = leftSide
            , rightSide = rightSide
            , body = body
            , haveSetBarrier = False
            }
    in
        ( model
        , Cmd.none
        )


update : CtrlMsg Msg -> Model -> ( Model, Cmd (CtrlMsg Msg) )
update msg model =
    ( model, Cmd.none )


animate : Ground -> Time -> Model -> ( Model, Cmd (CtrlMsg Msg) )
animate ground dt model =
    let
        needRelocation pos =
            let
                wantY =
                    model.startY + ground.elevation pos
            in
                if model.haveSetBarrier && V3.getY pos == wantY then
                    Nothing
                else
                    Just <| V3.setY wantY pos

        setPosition body pos =
            { body | position = pos }

        result newPos =
            let
                newModel =
                    { model
                        | body = setPosition model.body newPos
                        , haveSetBarrier = True
                    }

                addFloor =
                    Task.succeed (findBarrier newModel)
                        |> Task.perform (Effect << AddBarrier ())
            in
                ( newModel, addFloor )
    in
        case needRelocation model.body.position of
            Nothing ->
                ( model, Cmd.none )

            Just newPos ->
                result newPos


bodies : Model -> Vec3 -> List Body
bodies model pos =
    [ model.body ]


overlay : Model -> Html msg
overlay _ =
    Html.text "Road"



----------------------------------------------------------------------


findBarrier : Model -> Barrier
findBarrier model =
    List.map2 (,) model.leftSide model.rightSide
        |> mapPair (always Nothing)
            (\( a, b ) ( d, c ) -> Just (Quad a.position b.position c.position d.position))
        |> Maybe.values
        |> barrierFromQuads Grass
        |> relativeBarrier model.body.position



----------------------------------------------------------------------


type alias RoadVertex =
    { position : Vec3
    , normal : Vec3
    , coord : Vec3
    }


generateRoad : List RoadVertex -> List RoadVertex -> Vec3 -> Body
generateRoad leftSide rightSide startPos =
    let
        appear =
            roadAppearance leftSide rightSide
    in
        { anchor = AnchorGround
        , scale = vec3 1 1 1
        , position = startPos
        , orientation = Orientation.initial
        , appear = appear
        }


roadAppearance : List RoadVertex -> List RoadVertex -> Perception -> List Entity
roadAppearance leftSide rightSide p =
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
            roadMesh leftSide rightSide
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


roadMesh : List RoadVertex -> List RoadVertex -> Mesh NoiseVertex
roadMesh leftSide rightSide =
    let
        toNoiseVertex v =
            { position = v.position
            , normal = v.normal
            , coord = v.coord
            , color = vec4 0.4 0.4 0.4 0.5
            , smoothing = 0.1
            , textureScale = 0.2
            , timeScale = 0.0
            }
    in
        mkStrip
            (List.map toNoiseVertex leftSide)
            (List.map toNoiseVertex rightSide)
            |> triangleStrip


toRoadVertices : List BankingPoint -> List RoadVertex
toRoadVertices path =
    let
        thickness =
            0.01

        addThickness v =
            let
                y =
                    V3.getY v
            in
                V3.setY (y + thickness) v

        start v1 v2 =
            { position = addThickness v1.position
            , normal =
                uprightCross v1 v2
                    |> bankNormal v1.banking
            , coord = vec3 0 0 0
            }

        end v1 v2 prev =
            { position = addThickness v2.position
            , normal =
                uprightCross v1 v2
                    |> bankNormal v2.banking
            , coord = nextCoord v1.position v2.position prev.coord
            }

        middle v1 v2 v3 prev =
            { position = addThickness v2.position
            , normal =
                interpolateCross v1 v2 v3
                    |> bankNormal v2.banking
            , coord = nextCoord v1.position v2.position prev.coord
            }
    in
        foldTriple start end middle path


nextCoord : Vec3 -> Vec3 -> Vec3 -> Vec3
nextCoord v1 v2 prevCoord =
    let
        d =
            V3.distance v1 v2

        y =
            V3.getY prevCoord
    in
        V3.setY (y + d) prevCoord


bankNormal : Float -> ( Vec3, Vec3 ) -> Vec3
bankNormal banking ( upright, crosswalk ) =
    let
        axis =
            V3.cross upright crosswalk

        o =
            Orientation.fromAngleAxis (degrees banking) axis
    in
        Orientation.rotateBodyV o upright


uprightCross : BankingPoint -> BankingPoint -> ( Vec3, Vec3 )
uprightCross v1 v2 =
    let
        rise =
            V3.normalize (V3.sub v2.position v1.position)

        crosswalk =
            V3.cross (vec3 0 -1 0) rise
    in
        ( V3.cross crosswalk rise, crosswalk )


interpolateCross : BankingPoint -> BankingPoint -> BankingPoint -> ( Vec3, Vec3 )
interpolateCross v1 v2 v3 =
    let
        ( norm12, cross12 ) =
            uprightCross v1 v2

        ( norm23, cross23 ) =
            uprightCross v2 v3

        mean u1 u2 =
            V3.scale 0.5 (V3.add u1 u2)
    in
        ( mean norm12 norm23, mean cross12 cross23 )


roadSides : Float -> List RoadVertex -> ( List RoadVertex, List RoadVertex )
roadSides roadWidth path =
    ( side (-roadWidth / 2) path, side (roadWidth / 2) path )


side : Float -> List RoadVertex -> List RoadVertex
side sideWidth path =
    let
        mapX : (Float -> Float) -> Vec3 -> Vec3
        mapX f v =
            V3.setX (f (V3.getX v)) v

        start v1 v2 =
            { v1
                | position = startOffset sideWidth v1 v2
                , coord = mapX (\x -> x + sideWidth) v1.coord
            }

        end v1 v2 prev =
            { v2
                | position = endOffset sideWidth v1 v2
                , coord = mapX (\x -> x + sideWidth) v2.coord
            }

        middle v1 v2 v3 prev =
            { v2
                | position = corner sideWidth v1 v2 v3
                , coord = mapX (\x -> x + sideWidth) v2.coord
            }
    in
        foldTriple start end middle path



-- Given a path segment (v1, v2), return the offset of the roadside
-- ie. if the center of the road is the line v1->v2, then the
-- roadside passes through the line (v1+offset)->(v2+offset)


sideOffset : Float -> Vec3 -> Vec3 -> Vec3 -> Vec3
sideOffset sideWidth v1 v2 n =
    let
        segmentUnit =
            V3.normalize (V3.sub v2 v1)

        o =
            V3.cross segmentUnit n
    in
        V3.scale sideWidth o


startOffset : Float -> RoadVertex -> RoadVertex -> Vec3
startOffset sideWidth v1 v2 =
    V3.add v1.position (sideOffset sideWidth v1.position v2.position v1.normal)


endOffset : Float -> RoadVertex -> RoadVertex -> Vec3
endOffset sideWidth v1 v2 =
    V3.add v2.position (sideOffset sideWidth v1.position v2.position v2.normal)



-- Position of the corner of the roadside
-- Given a path (v1, v2, v3), return the position of the corner
-- near v2


corner : Float -> RoadVertex -> RoadVertex -> RoadVertex -> Vec3
corner sideWidth v1 v2 v3 =
    let
        offset12 =
            sideOffset sideWidth v1.position v2.position v2.normal

        pos12_1 =
            V3.add v2.position offset12

        offset23 =
            sideOffset sideWidth v2.position v3.position v2.normal

        pos23_0 =
            V3.add v2.position offset23
    in
        V3.scale 0.5 (V3.add pos12_1 pos23_0)



----------------------------------------------------------------------


mapPair : (a -> b) -> (a -> a -> b) -> List a -> List b
mapPair ending f xs =
    case xs of
        x1 :: x2 :: rest ->
            f x1 x2 :: mapPair ending f (x2 :: rest)

        [ x ] ->
            [ ending x ]

        [] ->
            []


mapTriple : (a -> a -> b) -> (a -> a -> b) -> (a -> a -> a -> b) -> List a -> List b
mapTriple start end middle xs0 =
    let
        f xs =
            case xs of
                x1 :: x2 :: x3 :: rest ->
                    middle x1 x2 x3 :: f (x2 :: x3 :: rest)

                [ x1, x2 ] ->
                    [ end x1 x2 ]

                _ ->
                    []
    in
        case xs0 of
            x1 :: x2 :: rest ->
                start x1 x2 :: f xs0

            _ ->
                []


foldTriple : (a -> a -> b) -> (a -> a -> b -> b) -> (a -> a -> a -> b -> b) -> List a -> List b
foldTriple start end middle xs0 =
    let
        f b0 xs =
            case xs of
                x1 :: x2 :: x3 :: rest ->
                    let
                        b =
                            middle x1 x2 x3 b0
                    in
                        b :: f b (x2 :: x3 :: rest)

                [ x1, x2 ] ->
                    [ end x1 x2 b0 ]

                _ ->
                    []
    in
        case xs0 of
            x1 :: x2 :: rest ->
                let
                    init =
                        start x1 x2
                in
                    init :: f init xs0

            _ ->
                []
