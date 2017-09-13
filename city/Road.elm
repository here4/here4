module Road
    exposing
        ()


import Here4.App as App exposing (..)
import Here4.App.Types exposing (..)
import Here4.Body exposing (..)
import Html exposing (Html)
import Html.Attributes as Html
import Math.Vector3 as V3 exposing (Vec3, vec3)


type alias Model =
    { path : List Vec3
    , bodies : List Body
    }

type alias Msg =
    ()


create : List Vec3 -> ( App, Cmd AppMsg )
create path =
    App.create (init path)
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


init : List Vec3 -> ( Model, Cmd (CtrlMsg Msg) )
init path =
    let
        bodies = generateRoad path
    in
        ( { path = path
          , bodies = bodies
          }
        , Cmd.none
        )


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


generateRoad : List Vec3 -> List Body
generateRoad path =
    []


roadMesh : List Vec3 -> List (Mesh RoadVertex)
roadMesh path =
    []


roadSides : Float -> List Vec3 -> (List Vec3, List Vec3)
roadSides roadWidth path =
    (side (-roadWidth/2) path, side (roadWidth/2) path)


side : Float -> List Vec3 -> List Vec3
side sideWidth path =
    case path of
        ( v1 :: v2 :: rest ) ->
            V3.add v1 (sideOffset sideWidth v1 v2) :: sideCorners sideWidth path
        _ ->
            []


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
    in
        V3.add v2 (V3.scale 0.5 (V3.add offset12 offset23))


-- Generate the corner vertices for a roadside, and the end
sideCorners : Float -> List Vec3 -> List Vec3
sideCorners sideWidth path =
    case path of
        ( v1 :: v2 :: v3 :: rest ) ->
            corner sideWidth v1 v2 v3 :: sideCorners (v2 :: v3 :: rest)
        [v1, v2] ->
            [v2]
        _ ->
            []
