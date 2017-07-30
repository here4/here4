module Vehicles.Walking exposing (drive, overlay)

import Color exposing (white)
import FontAwesome
import Here4.Orientation as Orientation exposing (..)
import Here4.Body exposing (..)
import Here4.Model as Model
import Here4.Ground exposing (Ground)
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
    let
        eyeLevel pos =
            ground.elevation pos + attributes.height
    in
        move attributes ground eyeLevel inputs thing


move : Driveable vehicle -> Ground -> Model.EyeLevel -> Model.Inputs -> Moving a -> Moving a
move attributes terrain eyeLevel inputs motion =
    motion
        |> turn eyeLevel attributes.speed inputs.x inputs.dt
        |> goForward eyeLevel attributes.speed inputs
        |> gravity eyeLevel inputs.dt
        |> physics eyeLevel inputs.dt
        |> keepWithinbounds terrain attributes.radius


turn : Model.EyeLevel -> Float -> Float -> Float -> Moving a -> Moving a
turn eyeLevel speed dx dt motion =
    let
        steer =
            0.1 * speed * dx * dt

        orientation =
            motion.orientation
                |> rollUpright
                |> pitchUpright
                |> followedBy (fromAngleAxis steer V3.j)
    in
        { motion | orientation = orientation }


goForward : Model.EyeLevel -> Float -> { i | rightTrigger : Float, leftTrigger : Float, mx : Float, y : Float, dt : Float } -> Moving a -> Moving a
goForward eyeLevel speed inputs motion =
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

        -- e = (eyeLevel motion.position) / 80.0 -- placement.yMult
        e =
            (eyeLevel motion.position) / 120.0

        friction =
            if e > 0.8 then
                -0.1
            else if e < 0.0 then
                0.8
            else if e < 0.1 then
                0.6
            else if e < 0.15 then
                0.5
            else
                0.2

        maxSpeed =
            if e > 0.8 then
                30
            else if e < 0.0 then
                3
            else if e < 0.1 then
                10
            else if e < 0.15 then
                15
            else
                20
    in
        { motion | velocity = adjustVelocity maxSpeed (8.0 * friction) (add move strafe) inputs.dt motion.velocity }


adjustVelocity : Float -> Float -> Vec3 -> Float -> Vec3 -> Vec3
adjustVelocity maxSpeed friction dv dt v =
    v3_clamp maxSpeed <| add (V3.scale dt dv) (V3.scale (1.0 - (friction * dt)) v)


physics : Model.EyeLevel -> Float -> Moving a -> Moving a
physics eyeLevel dt motion =
    let
        pos =
            add motion.position (Orientation.rotateBodyV motion.orientation (V3.scale dt motion.velocity))

        p =
            V3.toRecord pos

        e =
            eyeLevel pos

        vy0 =
            getY motion.velocity

        ( pos_, dv ) =
            if p.y < e then
                let
                    vy =
                        if ((e < (0.8 * 80) && vy0 > -30) || vy0 > -9.8) && e - p.y > (10 * dt) then
                            clamp 0 10 (V3.length motion.velocity * (e - p.y) * dt * 5)
                        else
                            0
                in
                    ( vec3 p.x e p.z, vec3 0 vy 0 )
            else
                ( pos, vec3 0 0 0 )
    in
        { motion | position = pos_, velocity = V3.add motion.velocity dv }



-- | Clamp a vector to be no longer than len


v3_clamp : Float -> Vec3 -> Vec3
v3_clamp len v =
    if V3.length v <= len then
        v
    else
        V3.scale len (V3.normalize v)


keepWithinbounds terrain radius motion =
    { motion | position = terrain.bounds radius motion.position }


gravity : Model.EyeLevel -> Float -> Moving a -> Moving a
gravity eyeLevel dt motion =
    if getY motion.position <= eyeLevel motion.position then
        motion
    else
        let
            v =
                V3.toRecord motion.velocity
        in
            { motion | velocity = vec3 v.x (v.y - 9.8 * dt) v.z }


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
