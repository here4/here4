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


drive : Driveable vehicle -> Vec3 -> Ground -> Model.Inputs -> Turning a -> Turning a
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


hovercraft : Driveable vehicle -> Vec3 -> Ground -> Model.Inputs -> Turning a -> Turning a
hovercraft attributes dimensions ground inputs thing =
    move Nothing attributes dimensions (aboveWater attributes.height ground) inputs thing


boat : Driveable vehicle -> Vec3 -> Ground -> Model.Inputs -> Turning a -> Turning a
boat attributes dimensions ground inputs thing =
    move (Just [ DeepWater, ShallowWater ]) attributes dimensions (aboveWater attributes.height ground) inputs thing


move : Maybe (List GroundSurface) -> Driveable vehicle -> Vec3 -> Ground -> Model.Inputs -> Turning a -> Turning a
move surfaces attributes dimensions ground inputs motion =
    let
{-
        tireFloor pos =
            let
                tirePos =
                    pos

                -- V3.add (vec3 0 0.1 0) pos
            in
                V3.getY tirePos - nearestFloor ground tirePos
-}

        go dt motion =
            let
                -- (partMotion, dtRemaining, applyGravity) =
                newMotion =
                    constrainSurfaces surfaces ground attributes.height dt
                       (updatePosition ground attributes.height dt) motion
                    -- updatePosition ground attributes.height dt motion

{-
                newMotion =
                    if applyGravity then
                        gravity ground (dt - dtRemaining) partMotion
                    else
                        partMotion
-}
            in
{-
                if dt < 0.01 && dtRemaining > 0.005 then
                    reorient ground attributes.height newMotion
                    |> go dtRemaining
                else
-}
                    newMotion

    in
        motion
            -- |> turn attributes dimensions tireFloor inputs.x inputs.dt
            |> inputOrientation attributes.speed inputs.x inputs.dt
            |> reorient ground attributes.height inputs.dt
            |> updateVelocity ground attributes.speed attributes.height inputs
            |> gravity ground attributes.height inputs.dt
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


inputOrientation : Float -> Float -> Float -> Turning a -> Turning a
inputOrientation speed dx dt motion =
    let
        steer =
            0.1 * speed * dx * dt

        up =
            Orientation.rotateBodyV motion.orientation V3.j

        orientationChange =
            fromAngleAxis steer up

        orientation =
            motion.orientation
            |> followedBy orientationChange

{-
        angularVelocity =
            motion.angularVelocity
            |> followedBy orientationChange
-}
    in
        { motion
            | orientation = orientation
            -- , angularVelocity = angularVelocity
        }


reorient : Ground -> Float -> Float -> Turning a -> Turning a
reorient ground height dt motion =
    let
        currentUp =
            Orientation.rotateBodyV motion.orientation V3.j

        ray =
            -- { origin = V3.add motion.position (V3.scale (height / 2) currentUp)
            { origin = motion.position
            -- , vector = V3.scale -(height*2) currentUp
            , vector = vec3 0 -height 0 -- actually want inertial down
            }
{-
        upright =
            motion.orientation
                |> Orientation.rollUpright
                |> Orientation.pitchUpright
-}

        qscale f q =
            Orientation.slerp f Orientation.initial q

        (orientation, angularVelocity) =
            case ground.barrier ray of
                Just barrierPoint ->
                    let
                        o = Orientation.fromTo currentUp barrierPoint.normal

                        s =
                            clamp 0.0 1.0 (0.01 * dt)

                        o2 =
                          qscale s o
                    in
                        ( Orientation.followedBy o motion.orientation
                        -- , Orientation.slerp s motion.angularVelocity (Orientation.followedBy o motion.angularVelocity)
                        , Orientation.followedBy o2 motion.angularVelocity
                        )
                Nothing ->
                    let
                        -- end =
                        --    Orientation.followedBy motion.angularVelocity motion.orientation
                        s =
                            clamp 0.0 1.0 (0.1 * dt)

                        o2 =
                            qscale s motion.angularVelocity
                    in
                        --( Orientation.slerp s motion.orientation end
                        ( Orientation.followedBy o2 motion.orientation
                        , motion.angularVelocity
                        )

{-
        orientation =
            motion.orientation
            |> Orientation.followedBy orientationChange

        angularVelocity =
            motion.angularVelocity
            |> followedBy angularVelocityChange
-}
    in
        { motion
            | orientation = orientation
            -- , angularVelocity = angularVelocity
        }

{-
turn : Driveable vehicle -> Vec3 -> (Vec3 -> Float) -> Float -> Float -> Turning a -> Turning a
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

updateVelocity : Ground -> Float -> Float -> { i | rightTrigger : Float, leftTrigger : Float, mx : Float, y : Float, dt : Float } -> Turning a -> Turning a
updateVelocity ground speed height inputs motion =
    let
        accel =
            if nearestFloor ground motion.position > height + 0.5 then
                0
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


-- constrainSurfaces : Maybe (List GroundSurface) -> Ground -> Float -> Float -> (Turning a -> (Turning a, Float, Bool)) ->  Turning a -> (Turning a, Float, Bool)
constrainSurfaces : Maybe (List GroundSurface) -> Ground -> Float -> Float -> (Turning a -> Turning a) ->  Turning a -> Turning a
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
                    motion

            Nothing ->
                f motion

-- updatePosition : Ground -> Float -> Float -> Turning a -> (Turning a, Float, Bool)
updatePosition : Ground -> Float -> Float -> Turning a -> Turning a
updatePosition ground height dt motion =
    let
        orientedVelocity =
            Orientation.rotateBodyV motion.orientation (V3.scale dt motion.velocity)
            |> V3.add (vec3 0 (-motion.gravityVelocity * dt) 0)

{-
        orientedVelocity =
            Orientation.rotateBodyV motion.orientation (V3.scale dt motion.velocity)

        currentUp =
            Orientation.rotateBodyV motion.orientation V3.j
-}

        forwardOrigin =
            -- V3.add motion.position (V3.scale (height / 2) currentUp)
            motion.position

        forwardRay =
            { origin = forwardOrigin
            , vector = orientedVelocity
            }

    in
        case ground.barrier forwardRay of
            Just barrierPoint ->
                let
                    currentUp =
                        Orientation.rotateBodyV motion.orientation V3.j

                in
                        if V3.dot barrierPoint.normal currentUp > 0.01 then
                            let
                                o =
                                    Orientation.fromTo currentUp (V3.normalize barrierPoint.normal)

                                newOrientation =
                                    motion.orientation
                                        |> Orientation.followedBy o

                                newOrientedVelocity =
                                    Orientation.rotateBodyV newOrientation (V3.scale dt motion.velocity)
                            in
                                { motion
                                    | position = V3.add motion.position newOrientedVelocity
                                    , orientation = newOrientation
                                }
                                    
                        else
                            { motion | position =
                                V3.sub barrierPoint.position (V3.scale height (V3.normalize orientedVelocity))
                            }
{-
                    newPosition =
                        if V3.dot barrierPoint.normal currentUp > 0.01 then
                            let
                                o =
                                    Orientation.fromTo currentUp barrierPoint.normal

                                newOrientation =
                                    motion.orientation
                                        |> Orientation.followedBy o

                                newOrientedVelocity =
                                    Orientation.rotateBodyV newOrientation (V3.scale dt motion.velocity)
                            in
                                V3.add motion.position newOrientedVelocity
                                    
                        else
                            V3.sub barrierPoint.position (V3.scale height (V3.normalize orientedVelocity))
{-
                    intersectRatio =
                        (V3.length orientedVelocity) /
                        (V3.distance forwardOrigin barrierPoint.position)

                    dtRemaining =
                        dt * (1.0 - intersectRatio)
-}
                in
                    -- ({ motion | position = newPosition }, dtRemaining, False)
                    { motion | position = newPosition }
-}
            Nothing ->
                let
                    wantPosition =
                        V3.add motion.position orientedVelocity

                    newDown =
                        Orientation.rotateBodyV motion.orientation (vec3 0 -(height-0.1) 0)

                    downRay =
                        { origin = wantPosition
                        , vector = newDown
                        }
                in
                    --{ motion | position = wantPosition }
                    case ground.barrier downRay of
                        Just b ->
                            let
                                n =
                                    if V3.dot newDown b.normal > 0 then
                                        V3.negate b.normal
                                    else
                                        b.normal

                                stepPosition =
                                    -- V3.add b.position (V3.scale height b.normal)
                                    V3.add b.position (V3.scale height n)

                                newPosition =
                                    -- If stepPosition is above wantPosition, step
                                    -- up to stepPosition
                                    if V3.dot n (V3.sub stepPosition wantPosition) > 0 then
                                        stepPosition
                                    else
                                        wantPosition
                            in
                                { motion | position = newPosition }

                        Nothing ->
                            { motion | position = wantPosition }
{-
                let
                    wantPosition =
                        V3.add motion.position orientedVelocity
{-
                    newDown =
                        Orientation.rotateBodyV motion.orientation (vec3 0 -height 0)

                    downRay =
                        { origin = V3.sub wantPosition (V3.scale 0.5 newDown)
                        , vector = newDown
                        }
-}
                in
                    { motion | position = wantPosition }
{-
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
                                ({ motion | position = newPosition }, 0, False)

                        Nothing ->
                            ({ motion | position = wantPosition }, 0, True)
-}
-}

{-
physicsOld : Ground -> Float -> Float -> Turning a -> Turning a
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

gravity : Ground -> Float -> Float -> Turning a -> Turning a
gravity ground height dt motion =
    let
        p =
            V3.toRecord motion.position

        altitude =
            nearestFloor ground motion.position

        fall =
            min (9.8 * dt) (altitude - height)
    in
        if (altitude <= height + 0.02) then
            { motion | gravityVelocity = Debug.log "gravity" 0 }
        else
            -- { motion | position = vec3 p.x (p.y - fall) p.z }
            { motion | gravityVelocity = Debug.log "gravity" (motion.gravityVelocity + fall) }

{-
gravity : Ground -> Float -> Float -> Turning a -> Turning a
gravity ground height dt motion =
    let
        p =
            V3.toRecord motion.position

        altitude =
            nearestFloor ground motion.position

        fall =
            min (9.8 * dt) (altitude - height)
    in
        if (altitude <= height + 0.01) then
            motion
        else
            { motion | position = vec3 p.x (p.y - fall) p.z }
-}

{-
gravity : Ground -> Float -> Float -> Turning a -> Turning a
gravity ground height dt motion =
    if nearestFloor ground motion.position <= height then
        motion
    else
        let
            v =
                V3.toRecord motion.velocity
        in
            { motion | velocity = vec3 v.x (v.y - 9.8 * dt) v.z }
-}


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
