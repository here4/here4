module View exposing (view)

import Color exposing (black, white)
import FontAwesome


-- import Html

import Html exposing (Html, text, div, p, span)
import Html.Attributes exposing (width, height, style)
import List.Extra as List
import Math.Matrix4 as M4
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as V3
import Math.Vector3 exposing (..)
import Time exposing (Time)
import WebGL
import Window
import App exposing (..)
import Appearance exposing (Appearance, Perception)
import Body exposing (Body)
import Camera exposing (Camera)
import Camera.Util exposing (cameraUp)
import Control exposing (WorldMsg)
import Methods exposing (..)
import Model exposing (Model, Msg(..), WorldKey(..))
import Orientation


{-| Generate a View from a Model
-}
view :
    Methods model msg
    -> Model model (WorldMsg msg)
    -> Html (Msg (WorldMsg msg))
view methods model =
    case model.maybeWindowSize of
        Nothing ->
            text "Starting ..."

        Just windowSize ->
            layoutScene windowSize methods model


layoutScene :
    Window.Size
    -> Methods worldModel msg
    -> Model worldModel (WorldMsg msg)
    -> Html (Msg (WorldMsg msg))
layoutScene windowSize methods model =
    let
        toUnit (WorldKey n _) = WorldKey n ()

        mRender player =
            Maybe.map toUnit player.partyKey
            |> Maybe.andThen (\worldKey -> methods.view worldKey model.worldModel)
            |> Maybe.map (renderWorld model.globalTime)

        worldLabel player =
            Maybe.map toUnit player.partyKey
            |> Maybe.andThen (\worldKey -> methods.worldLabel worldKey model.worldModel)
            |> Maybe.withDefault "(Nowhere)"

        worldLabel1 =
            worldLabel model.player1

        worldLabel2 =
            worldLabel model.player2

        mRender1 =
            mRender model.player1

        mRender2 =
            mRender model.player2

        orLoading f m =
            Maybe.withDefault (loading windowSize) (Maybe.map f m)

    in
        if model.player1.cameraVR then
            orLoading (layoutSceneVR windowSize model) mRender1
        else if model.numPlayers == 2 then
            case (mRender1, mRender2) of
                (Just render1, Just render2) ->
                    layoutScene2 windowSize model
                        worldLabel1 render1
                        worldLabel2 render2
                _ ->
                    loading windowSize
        else
            orLoading (layoutScene1 windowSize model worldLabel1) mRender1


loading : Window.Size -> Html (Msg worldMsg)
loading windowSize =
    let
        left = 0
        right = 0
        helpHMargin = windowSize.width//10
        helpVMargin = windowSize.height//10

        overlayContent = text "Loading ..."
    in
        overlay left right helpHMargin helpVMargin overlayContent


type alias RenderWorld msg =
    Model.Eye -> Window.Size -> Model.Player msg -> List WebGL.Entity

layoutScene1 : Window.Size -> Model worldModel worldMsg -> String -> RenderWorld worldMsg -> Html (Msg worldMsg)
layoutScene1 windowSize model worldLabel render =
    div
        [ style
            [ ( "width", toString windowSize.width ++ "px" )
            , ( "height", toString windowSize.height ++ "px" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ WebGL.toHtml
            [ width windowSize.width
            , height windowSize.height
            , style
                [ ( "display", "block" )
                , ( "position", "absolute" )
                , ( "top", "0px" )
                , ( "left", "0px" )
                , ( "right", "0px" )
                ]
            ]
            (render Model.OneEye windowSize model.player1)
        , hud worldLabel model.paused model.player1 0 0 (windowSize.width//10) (windowSize.height//10)
        ]


layoutScene2 : Window.Size -> Model worldModel worldMsg -> String -> RenderWorld worldMsg -> String -> RenderWorld worldMsg -> Html (Msg worldMsg)
layoutScene2 windowSize model worldLabel1 render1 worldLabel2 render2 =
    let
        w2 =
            windowSize.width // 2

        ws2 =
            { windowSize | width = w2 }
    in
        div
            [ style
                [ ( "width", toString windowSize.width ++ "px" )
                , ( "height", toString windowSize.height ++ "px" )
                , ( "backgroundColor", "rgb(135, 206, 235)" )
                ]
            ]
            [ span []
                [ div []
                    [ WebGL.toHtml
                        [ width w2
                        , height windowSize.height
                        , style
                            [ ( "display", "block" )
                            , ( "float", "left" )
                            , ( "position", "absolute" )
                            , ( "top", "0px" )
                            , ( "left", "0px" )
                            , ( "right", toString w2 ++ "px" )
                            , ( "border", "0px" )
                            , ( "padding", "0px" )
                            ]
                        ]
                        (render1 Model.OneEye ws2 model.player1)
                    , hud worldLabel1 model.paused model.player1 0 w2 (windowSize.width//20) (windowSize.height//10)
                    ]
                , div []
                    [ WebGL.toHtml
                        [ width w2
                        , height windowSize.height
                        , style
                            [ ( "display", "block" )
                            , ( "float", "right" )
                            , ( "position", "absolute" )
                            , ( "top", "0px" )
                            , ( "left", toString w2 ++ "px" )
                            , ( "right", "0px" )
                            , ( "border", "0px" )
                            , ( "padding", "0px" )
                            ]
                        ]
                        (render2 Model.OneEye ws2 model.player2)
                    , hud worldLabel2 model.paused model.player2 w2 0 (windowSize.width//20) (windowSize.height//10)
                    ]
                ]
            ]


layoutSceneVR : Window.Size -> Model worldModel worldMsg -> RenderWorld worldMsg -> Html (Msg worldMsg)
layoutSceneVR windowSize model render =
    let
        w2 =
            windowSize.width // 2

        ws2 =
            { windowSize | width = w2 }
    in
        div
            [ style
                [ ( "width", toString windowSize.width ++ "px" )
                , ( "height", toString windowSize.height ++ "px" )
                , ( "backgroundColor", "rgb(135, 206, 235)" )
                ]
            ]
            [ WebGL.toHtml
                [ width w2
                , height windowSize.height
                , style
                    [ ( "display", "block" )
                    , ( "float", "left" )
                    ]
                ]
                (render Model.LeftEye ws2 model.player1)
            , WebGL.toHtml
                [ width w2
                , height windowSize.height
                , style
                    [ ( "display", "block" )
                    , ( "float", "right" )
                    ]
                ]
                (render Model.RightEye ws2 model.player1)
            ]


bodyAppear : Mat4 -> Perception -> Body -> List WebGL.Entity
bodyAppear skyLookAt p body =
    let
        rotate =
            Appearance.transform (Orientation.rotateLabM4 body.orientation)

        translate =
            Appearance.transform (M4.translate body.position)

        scale =
            Appearance.transform (M4.scale body.scale)

        appear =
            case body.anchor of
                Body.AnchorGround ->
                    body.appear
                        |> scale
                        >> rotate
                        >> translate

                Body.AnchorSky ->
                    body.appear
                        |> scale
                        >> rotate

                Body.AnchorHUD ->
                    body.appear
                        |> scale

        skyPerception =
            { p | lookAt = skyLookAt }
    in
        case body.anchor of
            Body.AnchorGround ->
                appear p

            Body.AnchorSky ->
                appear skyPerception

            Body.AnchorHUD ->
                appear skyPerception


eyeOffset : Model.Eye -> Camera -> Vec3
eyeOffset eye camera =
    if eye == Model.LeftEye then
        -- Orientation.rotateLabV camera.orientation (vec3 (-0.04) 0 0)
        Orientation.rotateBodyV camera.orientation (vec3 (-0.04) 0 0)
    else if eye == Model.RightEye then
        -- Orientation.rotateLabV camera.orientation (vec3 0.04 0 0)
        Orientation.rotateBodyV camera.orientation (vec3 0.04 0 0)
    else
        vec3 0 0 0


aboveGround : Model.EyeLevel -> Vec3 -> Vec3
aboveGround eyeLevel pos =
    let
        p =
            toRecord pos

        e =
            eyeLevel pos
    in
        if p.y < e then
            vec3 p.x e p.z
        else
            pos


{-| Set up 3D world
-}
renderWorld : Time -> Model.World -> Model.Eye -> Window.Size -> Model.Player msg -> List WebGL.Entity
renderWorld globalTime world eye windowSize player =
    let
        eyeLevel pos =
            Model.eyeLevel + world.ground.elevation pos

        lensDistort =
            if player.cameraVR then
                0.85
            else
                0.95

        p =
            { cameraPos = player.camera.position
            , perspective = perspective windowSize player
            , lookAt = lookAtBody windowSize player eye
            , globalTime = globalTime
            , windowSize = windowSize
            , lensDistort = lensDistort
            , cameraVR = player.cameraVR
            , measuredFPS = 30.0
            }

        skyLookAt =
            lookAtSky windowSize player

        appears =
            List.concat <| List.map (bodyAppear skyLookAt p) world.bodies
    in
        appears


{-| Calculate the viewer's field of view
-}
perspective : Window.Size -> Model.Player msg -> Mat4
perspective { width, height } player =
    M4.makePerspective player.camera.fovy (toFloat width / toFloat height) 0.01 1000

lookAtBody : Window.Size -> Model.Player msg -> Model.Eye -> Mat4
lookAtBody { width, height } player eye =
    M4.makeLookAt (add player.camera.position (eyeOffset eye player.camera))
        (add player.camera.position (scale 3 (Model.direction player.camera)))
        (cameraUp player.camera)


lookAtSky : Window.Size -> Model.Player msg -> Mat4
lookAtSky { width, height } player =
    M4.makeLookAt (vec3 0 0 0)
        (scale 3 (Model.direction player.camera))
        (cameraUp player.camera)


hud : String -> Bool -> Model.Player worldMsg -> Int -> Int -> Int -> Int -> Html (Msg worldMsg)
hud worldLabel paused player left right helpHMargin helpVMargin =
    let
        shotLabel = Maybe.map .label player.shot
                    |> Maybe.withDefault ""
        pausedLabel = if paused then " (Paused)" else ""

        showOverlay =
            if player.overlayVisible then
                overlay left right helpHMargin helpVMargin player.overlayContent
            else
                Html.text ""
    in
        div [] [
          div
            [ style
                [ ( "position", "absolute" )
                , ( "font-family", "Verdana, Geneva, sans-serif" )
                , ( "text-align", "center" )
                , ( "left", toString left ++ "px" )
                , ( "right", toString right ++ "px" )
                , ( "top", "0px" )
                , ( "border", "0px" )
                , ( "padding", "0px" )
                , ( "background-color", "rgba(0,0,0,0.5)" )
                , ( "color", "#fff" )
                , ( "font-size", "xx-large" )
                , ( "text-shadow", "1px 0 0 #000, 0 -1px 0 #000, 0 1px 0 #000, -1px 0 0 #000" )
                , ( "z-index", "1" )
                ]
            ]
            [ span []
                [ Html.text worldLabel
                , Html.text " "
                , FontAwesome.globe white 20
                , Html.text " "
                , Html.text player.rideLabel
                , Html.text " "
                , FontAwesome.diamond white 20
                , Html.text " "
                , Html.text shotLabel
                , Html.text " Camera"
                , Html.text pausedLabel
                ]
            ]
          ,
          showOverlay
        ]

overlay : Int -> Int -> Int -> Int -> Html worldMsg -> Html (Msg worldMsg)
overlay left right hMargin vMargin content =
        div
            [ style
                [ ( "position", "absolute" )
                , ( "font-family", "Verdana, Geneva, sans-serif" )
                , ( "text-align", "center" )
                , ( "left", toString (left + hMargin) ++ "px" )
                , ( "right", toString (right + hMargin) ++ "px" )
                , ( "top", toString vMargin ++ "px" )
                , ( "bottom", toString vMargin ++ "px" )
                , ( "border", "0px" )
                , ( "padding", "0px" )
                , ( "background-color", "rgba(0,0,0,0.5)" )
                , ( "color", "#fff" )
                , ( "font-size", "xx-large" )
                , ( "text-shadow", "1px 0 0 #000, 0 -1px 0 #000, 0 1px 0 #000, -1px 0 0 #000" )
                , ( "z-index", "1" )
                ]
            ]
            [ Html.map WorldMessage content
            ]



enterMsg : List (Html Msg)
enterMsg =
    message "Click to go full screen and move your head with the mouse."


exitMsg : List (Html Msg)
exitMsg =
    message "Press <escape> to exit full screen."


message : String -> List (Html Msg)
message msg =
    [ p [] [ Html.text "Use gamepad, arrows or WASD keys to move." ]
    , p [] [ Html.text msg ]
    ]
