module Here4.Vehicle.DreamBuggy exposing (drive, hovercraft, boat, overlay)

import Color exposing (white)
import FontAwesome
import Here4.Barrier exposing (..)
import Here4.Body exposing (..)
import Here4.Ground exposing (..)
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
-- DreamBuggy


welcome : Model.Motion -> Model.Motion
welcome motion =
    { motion | orientation = clampBuggy motion.orientation }


drive : Driveable vehicle -> Vec3 -> Ground -> Model.Inputs -> Moving a -> Moving a
drive =
    move Nothing


aboveWater : Float -> Ground -> Ground
aboveWater hover ground =
    let
        elevation pos =
            hover + max ground.seaLevel (ground.elevation pos)

        waterBounds : Float -> Vec3 -> Vec3
        waterBounds radius pos =
            let
                ( px, py, pz ) =
                    V3.toTuple (ground.bounds radius pos)
            in
                vec3 px (max (ground.seaLevel + radius) py) pz
    in
        { ground
            | bounds = waterBounds
            , elevation = elevation
        }


hovercraft : Driveable vehicle -> Vec3 -> Ground -> Model.Inputs -> Moving a -> Moving a
hovercraft attributes dimensions ground inputs thing =
    move Nothing attributes dimensions (aboveWater attributes.height ground) inputs thing


boat : Driveable vehicle -> Vec3 -> Ground -> Model.Inputs -> Moving a -> Moving a
boat attributes dimensions ground inputs thing =
    move (Just [ DeepWater, ShallowWater ]) attributes dimensions (aboveWater attributes.height ground) inputs thing


move : Maybe (List GroundSurface) -> Driveable vehicle -> Vec3 -> Ground -> Model.Inputs -> Moving a -> Moving a
move surfaces attributes dimensions ground inputs motion =
    let
        tireFloor pos =
            let
                tirePos =
                    pos

                -- V3.add (vec3 0 0.1 0) pos
            in
                V3.getY tirePos - nearestFloor ground tirePos

        go dt motion =
            let
                (partMotion, dtRemaining) =
                    constrainSurfaces surfaces ground attributes.height dt
                       (physics ground attributes.height dt) motion

                newMotion =
                    gravity ground (dt - dtRemaining) partMotion
            in
                if dtRemaining > 0.005 then
                    reorient ground attributes.height newMotion
                    |> go dtRemaining
                else
                    newMotion

    in
        motion
            -- |> turn attributes dimensions tireFloor inputs.x inputs.dt
            |> goForward ground attributes.speed inputs
            |> turn attributes.speed inputs.x inputs.dt
            |> reorient ground attributes.height
            |> go inputs.dt
{-
            |> constrainSurfaces surfaces ground attributes.height inputs.dt
                   (physics ground attributes.height inputs.dt)
-}
            |> keepWithinbounds ground attributes.radius


clampBuggy : Orientation -> Orientation
clampBuggy o =
    o



{-
   let (roll, pitch, yaw) = Orientation.toRollPitchYaw o
       roll_ = clamp (degrees -10) (degrees 10) (roll/2)
       pitch_ = clamp (degrees -15) (degrees 15) (pitch/2)
   in Orientation.fromRollPitchYaw (roll_, pitch_, yaw)
-}


flatten : Vec3 -> Vec3
flatten v =
    let
        r =
            V3.toRecord v
    in
        normalize (vec3 r.x 0 r.z)


turn : Float -> Float -> Float -> Moving a -> Moving a
turn speed dx dt motion =
    let
        steer =
            0.1 * speed * dx * dt

        up =
            Orientation.rotateBodyV motion.orientation V3.j

        orientation =
            motion.orientation
                |> followedBy (fromAngleAxis steer up)
    in
        { motion | orientation = orientation }


reorient : Ground -> Float -> Moving a -> Moving a
reorient ground height motion =
    let
        currentUp =
            Orientation.rotateBodyV motion.orientation V3.j

        ray =
            { origin = V3.add motion.position (V3.scale (height / 2) currentUp)
            , vector = V3.scale -height currentUp
            }

        upright =
            motion.orientation
                |> Orientation.rollUpright
                |> Orientation.pitchUpright

        orientation =
            case ground.barrier ray of
                Just barrierPoint ->
                    let
                        o =
                            Orientation.fromTo currentUp barrierPoint.normal
                    in
                        motion.orientation
                            |> Orientation.followedBy o

                Nothing ->
                    upright
    in
        { motion | orientation = orientation }

{-
turn : Driveable vehicle -> Vec3 -> (Vec3 -> Float) -> Float -> Float -> Moving a -> Moving a
turn attributes dimensions tireFloor dx dt motion =
    let
        vehicleWidth =
            V3.getX dimensions

        vehicleFwdLength =
            V3.getZ dimensions

        w =
            vehicleWidth / 2.0

        l =
            vehicleFwdLength / 2.0

        centerY =
            tireFloor motion.position

        frontRightTireY =
            tireFloor (add motion.position (rotateBodyV motion.orientation (vec3 w 0 l)))

        frontLeftTireY =
            tireFloor (add motion.position (rotateBodyV motion.orientation (vec3 w 0 -l)))

        rearRightTireY =
            tireFloor (add motion.position (rotateBodyV motion.orientation (vec3 -w 0 l)))

        rearLeftTireY =
            tireFloor (add motion.position (rotateBodyV motion.orientation (vec3 -w 0 -l)))

        frontTireY =
            max frontLeftTireY frontRightTireY

        rearTireY =
            max rearLeftTireY rearRightTireY

        rightTireY =
            max frontRightTireY rearRightTireY

        leftTireY =
            max frontRightTireY rearRightTireY

        perp2dCCW x y =
            ( -y, x )

        perp2dCW x y =
            ( y, -x )

        targetUpRoll =
            let
                ( x, y ) =
                    perp2dCW vehicleWidth (rightTireY - leftTireY)
            in
                vec3 x y 0

        targetUpPitch =
            let
                ( z, y ) =
                    if frontTireY > centerY then
                        perp2dCW vehicleFwdLength (frontTireY - rearTireY)
                    else
                        perp2dCW (vehicleFwdLength / 2.0) (centerY - rearTireY)
            in
                vec3 0 y z

        steer =
            -- 0.4 * dx * dt
            2.0 * dx * dt

        targetOrientation =
            if getY motion.position > (tireFloor motion.position) + 0.5 then
                -- spin if in the air
                motion.orientation
                    |> rollUpright
                    |> pitchUpright
                    |> followedBy (fromAngleAxis (5.0 * steer) V3.j)
            else
                motion.orientation
                    |> rollTo targetUpRoll
                    |> pitchTo targetUpPitch
                    |> followedBy (fromAngleAxis steer V3.j)

        orientation =
            targetOrientation
    in
        { motion | orientation = orientation }
-}

goForward : Ground -> Float -> { i | rightTrigger : Float, leftTrigger : Float, mx : Float, y : Float, dt : Float } -> Moving a -> Moving a
goForward ground speed inputs motion =
    let
        accel =
            if getY motion.position > (ground.elevation motion.position) + 0.5 then
                -0.1
            else
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
        { motion | velocity = adjustVelocity ((sqrt speed) * maxSpeed) friction (add move strafe) inputs.dt motion.velocity }


adjustVelocity : Float -> Float -> Vec3 -> Float -> Vec3 -> Vec3
adjustVelocity maxSpeed friction dv dt v =
    v3_clamp maxSpeed <| add (V3.scale dt dv) (V3.scale (1.0 - (friction * dt)) v)


constrainSurfaces : Maybe (List GroundSurface) -> Ground -> Float -> Float -> (Moving a -> (Moving a, Float)) ->  Moving a -> (Moving a, Float)
constrainSurfaces mSurfaces ground height dt f motion =
    let
        pos =
            V3.add motion.position (Orientation.rotateBodyV motion.orientation (V3.scale dt motion.velocity))

        topPos =
            V3.add (vec3 0 height 0) pos

        targetSurface =
            ground.surface topPos
    in
        case mSurfaces of
            Just surfaces ->
                if List.member targetSurface surfaces then
                    f motion
                else
                    (motion, 0)

            Nothing ->
                f motion

physics : Ground -> Float -> Float -> Moving a -> (Moving a, Float)
physics ground height dt motion =
    let
        orientedVelocity =
            Orientation.rotateBodyV motion.orientation (V3.scale dt motion.velocity)

        currentUp =
            Orientation.rotateBodyV motion.orientation V3.j

        forwardOrigin =
            V3.add motion.position (V3.scale (height / 2) currentUp)

        forwardRay =
            { origin = forwardOrigin
            , vector = orientedVelocity
            }

        (newMotion, dtRemaining) =
            case ground.barrier forwardRay of
                Just barrierPoint ->
                    let
                        newPosition =
                            V3.add barrierPoint.position (V3.scale 0.01 barrierPoint.normal)

                        intersectRatio =
                            (V3.length orientedVelocity) /
                            (V3.distance forwardOrigin barrierPoint.position)

                        dtRemaining =
                            dt * (1.0 - intersectRatio)
                    in
                        ({ motion | position = newPosition }, dtRemaining)

                Nothing ->
                    let
                        wantPosition =
                            V3.add motion.position orientedVelocity

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
                                    ({ motion | position = newPosition }, 0)

                            Nothing ->
                                ({ motion | position = wantPosition }, 0)
    in
        (newMotion, dtRemaining)

{-
physicsOld : Ground -> Float -> Float -> Moving a -> Moving a
physicsOld ground height dt motion =
    let
        pos =
            V3.add motion.position (Orientation.rotateBodyV motion.orientation (V3.scale dt motion.velocity))

        topPos =
            V3.add (vec3 0 height 0) pos

        p =
            V3.toRecord pos

        e =
            V3.getY topPos - nearestFloor ground topPos

        vy0 =
            getY motion.velocity

        ( pos_, dv ) =
            if p.y < e then
                let
                    vy =
                        -- if ((e < (0.8 * 80) && vy0 > -30) || vy0 > -9.8) && e - p.y > (10 * dt) then
                        clamp 0 10 (V3.length motion.velocity * (e - p.y) * dt * 5)

                    -- else
                    --     0
                in
                    ( vec3 p.x e p.z, vec3 0 vy 0 )
            else
                ( pos, vec3 0 0 0 )
    in
        { motion | position = pos_, velocity = add motion.velocity dv }
-}


-- | Clamp a vector to be no longer than len


v3_clamp : Float -> Vec3 -> Vec3
v3_clamp len v =
    if V3.length v <= len then
        v
    else
        V3.scale len (V3.normalize v)


keepWithinbounds ground radius motion =
    { motion | position = ground.bounds radius motion.position }


gravity : Ground -> Float -> Moving a -> Moving a
gravity ground dt motion =
    if nearestFloor ground motion.position <= 0.0 then
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
                [ Html.i [] [ Html.text "Dreambuggy" ]
                , Html.text " controls:"
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
