module Here4.Vehicle.Walking exposing (drive, overlay)

import Color exposing (white)
import FontAwesome
import Here4.Barrier exposing (..)
import Here4.Body exposing (..)
import Here4.Ground exposing (Ground, nearestFloor)
import Here4.Model as Model
import Here4.Orientation as Orientation exposing (..)
import Here4.Vehicle exposing (Driveable)
import Html exposing (Html)
import Html.Attributes as Html
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Debug


----------------------------------------------------------------------
-- Walking


drive : Driveable vehicle -> Vec3 -> Ground -> Model.Inputs -> Moving a -> Moving a
drive attributes dimensions ground inputs thing =
    move attributes ground attributes.height inputs thing


move : Driveable vehicle -> Ground -> Float -> Model.Inputs -> Moving a -> Moving a
move attributes ground height inputs motion =
    motion
        |> turn ground attributes.speed height inputs.x inputs.dt
        |> goForward ground attributes.speed inputs
        |> gravity ground height inputs.dt
        |> physics ground height inputs.dt
        |> keepWithinbounds ground attributes.radius


turn : Ground -> Float -> Float -> Float -> Float -> Moving a -> Moving a
turn ground speed height dx dt motion =
    let
        steer =
            0.1 * speed * dx * dt

        currentUp =
            Orientation.rotateBodyV motion.orientation V3.j

        ray =
            { origin = V3.add motion.position (V3.scale (height/2) currentUp)
            , vector = V3.scale -height currentUp
            }

        upright =
            motion.orientation
            |> Orientation.rollUpright
            |> Orientation.pitchUpright

        newOrientation =
            case ground.barrier ray of
                Just barrierPoint ->
                    let
                        o = Orientation.fromTo currentUp barrierPoint.normal
                    in
                        motion.orientation
                        |> Orientation.followedBy o
                Nothing ->
                    upright

        newUp =
            Orientation.rotateBodyV newOrientation V3.j

        orientation =
            newOrientation
            |> followedBy (fromAngleAxis steer newUp)
    in
        { motion | orientation = orientation }


goForward : Ground -> Float -> { i | rightTrigger : Float, leftTrigger : Float, mx : Float, y : Float, dt : Float } -> Moving a -> Moving a
goForward ground speed inputs motion =
    let
        accel =
            clamp -1.0 1.0 <|
                inputs.y
                    + inputs.rightTrigger
                    - inputs.leftTrigger

        move =
            V3.scale (speed * accel) V3.k

        strafe =
            V3.scale (0.1 * speed * -inputs.mx) V3.i

        surface =
            ground.surface motion.position

        friction =
            if surface == Snow then
                -0.1
            else if surface == DeepWater then
                0.8
            else if surface == ShallowWater then
                0.6
            else if surface == Beach then
                0.5
            else
                0.2

        maxSpeed =
            if surface == Snow then
                30
            else if surface == DeepWater then
                3
            else if surface == ShallowWater then
                10
            else if surface == Beach then
                15
            else
                20
    in
        { motion | velocity = adjustVelocity maxSpeed (8.0 * friction) (add move strafe) inputs.dt motion.velocity }


adjustVelocity : Float -> Float -> Vec3 -> Float -> Vec3 -> Vec3
adjustVelocity maxSpeed friction dv dt v =
    v3_clamp maxSpeed <| add (V3.scale dt dv) (V3.scale (1.0 - (friction * dt)) v)


physics : Ground -> Float -> Float -> Moving a -> Moving a
physics ground height dt motion =
    let
        orientedVelocity =
            Orientation.rotateBodyV motion.orientation (V3.scale dt motion.velocity)

        currentUp =
            Orientation.rotateBodyV motion.orientation V3.j

        forwardRay =
            { origin = V3.add motion.position (V3.scale (height/2) currentUp)
            , vector = orientedVelocity
            }

        newMotion =
            case ground.barrier forwardRay of
                Just barrierPoint ->
                    let
                        newPosition =
                            V3.add barrierPoint.position (V3.scale 0.01 barrierPoint.normal)
                    in
                        { motion | position = newPosition }

                Nothing ->
                    let
                        wantPosition = V3.add motion.position orientedVelocity

                        newDown =
                            Orientation.rotateBodyV motion.orientation (vec3 0 -height 0)

                        downRay =
                            { origin = V3.sub wantPosition (V3.scale 0.5 newDown)
                            , vector = newDown
                            }
                    in
                            case ground.barrier downRay of
                                Just b ->
                                    let
                                        stepPosition =
                                            V3.add b.position (V3.scale 0.01 b.normal)

                                        newPosition =
                                            if V3.dot (V3.sub stepPosition wantPosition) orientedVelocity < 0 then
                                                wantPosition
                                            else
                                                stepPosition
                                    in
                                        { motion | position = newPosition }
                                Nothing ->
                                    { motion | position = wantPosition }
    in
        newMotion


-- | Clamp a vector to be no longer than len
v3_clamp : Float -> Vec3 -> Vec3
v3_clamp len v =
    if V3.length v <= len then
        v
    else
        V3.scale len (V3.normalize v)


keepWithinbounds ground radius motion =
    { motion | position = ground.bounds radius motion.position }


gravity : Ground -> Float -> Float -> Moving a -> Moving a
gravity ground height dt motion =
    let
        p =
            V3.toRecord motion.position

        altitude =
            nearestFloor ground motion.position

        fall =
            min (9.8 * dt) (altitude - 0.01)
    in
        if (altitude <= 0.015) then
            motion 
        else
            { motion | position = vec3 p.x (p.y - fall) p.z }


overlay : Html msg
overlay =
    let
        textStyle =
            Html.style
                [ ( "font-family", "Verdana, Geneva, sans-serif" )
                , ( "color", "#fff" )
                , ( "font-size", "xx-large" )
                , ( "text-shadow", "1px 0 0 #000, 0 -1px 0 #000, 0 1px 0 #000, -1px 0 0 #000" )
                , ( "z-index", "1" )
                ]

        textLeft =
            Html.style [ ( "text-align", "left" ) ]

        textCenter =
            Html.style [ ( "text-align", "center" ) ]
    in
        Html.div []
            [ Html.div
                [ textStyle
                , textLeft
                , Html.style
                    [ ( "margin-left", "3em" ) ]
                ]
                [ Html.text "Walking controls:"
                ]
            , Html.table
                [ textStyle
                , textCenter
                , Html.style
                    [ ( "width", "80%" )
                    , ( "height", "60%" )
                    , ( "margin", "auto" )
                    ]
                ]
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th [] [ Html.text "" ]
                        , Html.th [] [ FontAwesome.keyboard_o white 48 ]
                        , Html.th [] [ FontAwesome.gamepad white 48 ]
                        ]
                    ]
                , Html.tbody []
                    [ Html.tr []
                        [ Html.th [ textLeft ] [ Html.text "Accelerate / Brake" ]
                        , Html.td [] [ Html.text "W, S" ]
                        , Html.td []
                            [ Html.text "Right,left triggers"
                            , Html.br [] []
                            , Html.text "Right stick up, down"
                            ]
                        ]
                    , Html.tr []
                        [ Html.th [ textLeft ] [ Html.text "Steer left, right" ]
                        , Html.td [] [ Html.text "A, D" ]
                        , Html.td [] [ Html.text "Right stick left, right" ]
                        ]
                    , Html.tr []
                        [ Html.th [ textLeft ] [ Html.text "Slide left, right" ]
                        , Html.td [] [ Html.text "Arrow left, right" ]
                        , Html.td [] [ Html.text "Left stick left, right" ]
                        ]
                    ]
                ]
            ]
