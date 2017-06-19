module Vehicles.DreamBird exposing (drive, overlay)

import Html exposing (Html)
import Html.Attributes as Html
import FontAwesome
import Color exposing (white)
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Orientation exposing (..)
import Body exposing (..)
import Model
import Ground exposing (Ground)


----------------------------------------------------------------------
-- DreamBird
-- | Welcome a new driver to the DreamBird


welcome : Model.Motion -> Model.Motion
welcome motion =
    motion


drive : Ground -> Model.Inputs -> Moving (HasBody a) -> Moving (HasBody a)
drive ground inputs body =
    let
        eyeLevel pos =
            1.8 + ground.elevation pos

        motion0 =
            { position = body.position, orientation = body.orientation, velocity = body.velocity }

        motion =
            move ground eyeLevel inputs motion0
    in
        { body
            | position = motion.position
            , orientation = motion.orientation
            , velocity = motion.velocity
        }


move : Ground -> Model.EyeLevel -> Model.Inputs -> Model.Motion -> Model.Motion
move ground eyeLevel inputs motion =
    motion
        |> fly eyeLevel inputs
        |> flyPhysics eyeLevel inputs.dt
        |> keepWithinbounds ground



-- http://www.dtic.mil/dtic/tr/fulltext/u2/a152616.pdf


fly : Model.EyeLevel -> Model.Inputs -> Model.Motion -> Model.Motion
fly eyeLevel inputs motion =
    let
        thrust =
            clamp -1.0 1.0 <|
            inputs.my + inputs.rightTrigger - inputs.leftTrigger

        yaw =
            0.5 * inputs.mx * inputs.dt

        pitch =
            -0.8 * inputs.y * inputs.dt

        roll =
            1.2 * inputs.x * inputs.dt

        orpy =
            fromRollPitchYaw ( roll, pitch, yaw )

        orientation =
            followedBy motion.orientation orpy

        orient =
            rotateBodyV orientation

        dv =
            V3.scale (10 * thrust * inputs.dt) <| orient V3.k

        du =
            V3.scale (4 * thrust * inputs.dt) <| orient V3.j

        dv_ =
            add dv du

        vel =
            add (V3.scale 0.8 dv_) (V3.scale 0.95 motion.velocity)
    in
        { motion
            | orientation = orientation
            , velocity = vel
        }


flyPhysics : Model.EyeLevel -> Float -> Model.Motion -> Model.Motion
flyPhysics eyeLevel dt motion =
    let
        pos =
            add motion.position (V3.scale dt motion.velocity)

        p =
            toRecord pos

        e =
            eyeLevel pos

        ( pos_, dv ) =
            if p.y < e then
                ( vec3 p.x e p.z, vec3 0 0 0 )
            else
                ( pos, vec3 0 0 0 )
    in
        { motion | position = pos_, velocity = add motion.velocity dv }


keepWithinbounds ground motion =
    { motion | position = ground.bounds motion.position }


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
            [ Html.h3
                [ textStyle ]
                [ Html.text "Dreambird controls:" ]
            , Html.table
                [ textStyle
                , textCenter
                , Html.style
                    [ ( "width", "80%" )
                    , ( "margin", "auto" )
                    ]
                ]
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th [] [ Html.text "" ]
                        , Html.th [] [ FontAwesome.keyboard_o white 40 ]
                        , Html.th [] [ FontAwesome.gamepad white 40 ]
                        ]
                    ]
                , Html.tbody []
                    [ Html.tr []
                        [ Html.th [ textLeft ] [ Html.text "Thrust +/-" ]
                        , Html.td [] [ Html.text "Arrow up,down" ]
                        , Html.td []
                              [ Html.span [] [ Html.text "Right,left triggers" ]
                              , Html.span [] [ Html.text "Left stick up,down" ]
                              ]
                        ]
                    , Html.tr []
                        [ Html.th [ textLeft ] [ Html.text "Yaw left, right" ]
                        , Html.td [] [ Html.text "Arrow left, right" ]
                        , Html.td [] [ Html.text "Left stick left,right" ]
                        ]
                    , Html.tr []
                        [ Html.th [ textLeft ] [ Html.text "Pitch up,down" ]
                        , Html.td [] [ Html.text "W, S" ]
                        , Html.td [] [ Html.text "Right stick up,down" ]
                        ]
                    , Html.tr []
                        [ Html.th [ textLeft ] [ Html.text "Roll left, right" ]
                        , Html.td [] [ Html.text "A, D" ]
                        , Html.td [] [ Html.text "Right stick left,right" ]
                        ]
                    ]
                ]
    {-
            , Html.ul []
                [ Html.li [] [ Html.text "WASD / Right stick: pitch+roll" ]
                , Html.li [] [ Html.text "Arrows / Left stick: Thrust+yaw" ]
                ]
    -}
            ]
