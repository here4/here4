module View exposing (view)

import Html
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)
import Time exposing (Time)

import Model
import Things.Cube exposing (textureCube, fireCube, fogMountainsCube, voronoiCube)
import Things.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Things.Sphere exposing (cloudsSphere)
import Things.Ground exposing (renderGround)
import WebGL
import Html exposing (Html, text, div, p)
import Html.Attributes exposing (width, height, style)
import Model exposing (Model, Msg)
import Window

type alias Perception = {
    cameraPos  : Vec3,
    resolution : (Int, Int),
    globalTime : Time,
    viewMatrix : Mat4,
    lensDistort : Float,
    measuredFPS : Float
}

-- type alias See = Perception -> List Renderable

{-| Generate a View from a Model
-}
view : Model -> Html Msg
view { person, lifetime, maybeWindowSize, maybeTexture, isLocked } =
    case (maybeWindowSize, maybeTexture) of
        (Nothing, _) -> text ""
        (_, Nothing) -> text ""
        (Just windowSize, Just texture) ->
            layoutScene windowSize lifetime isLocked texture person

layoutScene : Window.Size -> Time -> Bool -> WebGL.Texture -> Model.Person -> Html Msg
layoutScene windowSize t isLocked texture person =
    div
        [ style
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            , ( "position", "relative" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ WebGL.toHtml
            [ width windowSize.width
            , height windowSize.height
            , style [ ( "display", "block" ) ]
            ]
            (renderWorld windowSize t texture (perspective windowSize person))
        , div
            [ style
                [ ( "position", "absolute" )
                , ( "font-family", "monospace" )
                , ( "text-align", "center" )
                , ( "left", "20px" )
                , ( "right", "20px" )
                , ( "top", "20px" )
                ]
            ]
            (if isLocked then
                exitMsg
             else
                enterMsg
            )
        ]

translateP position p = { p | viewMatrix = translate position p.viewMatrix }

{-| Set up 3D world
-}
renderWorld : Window.Size -> Time -> WebGL.Texture -> Mat4 -> List WebGL.Renderable
renderWorld windowSize t texture perspective =
    let
        p = { viewMatrix = perspective
            , globalTime = t
            , windowSize = windowSize
            , lensDistort = 0.9
            }
        worldObjects = List.concat
            [ fogMountainsDiamond (translateP (vec3 0 1.5 0) p)
            , cloudsDiamond (translateP (vec3 5 1.5 1) p)
            , cloudsSphere (translateP (vec3 3 10 5) p)
            , voronoiCube (translateP (vec3 10 0 10) p)
            , fireCube (translateP (vec3 -10 0 -10) p)
            , fogMountainsCube (translateP (vec3 10 1.5 -10) p)
            , textureCube texture (translateP (vec3 -2 0 -17) p)
            , renderGround p
            ]
    in
        worldObjects

{-| Calculate the viewer's field of view
-}
perspective : Window.Size -> Model.Person -> Mat4
perspective { width, height } person =
    mul (makePerspective 45 (toFloat width / toFloat height) 0.01 100)
        (makeLookAt person.position (person.position `add` Model.direction person) j)

enterMsg : List (Html Msg)
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : List (Html Msg)
exitMsg = message "Press <escape> to exit full screen."

message : String -> List (Html Msg)
message msg =
    [ p [] [ Html.text "WASD keys to move, space bar to jump." ]
    , p [] [ Html.text msg ]
    ]
