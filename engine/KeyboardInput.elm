module KeyboardInput exposing (..)

import Keyboard
import Model

{- Subscribe to keychange events. -}

keyChange : Bool -> Keyboard.KeyCode -> Model.Msg worldMsg
keyChange on keyCode =
    if keyCode == 27 && on then
        Model.LockRequest False
    else
        (case keyCode of
            9 ->
                \k -> { k | tab = on }

            16 ->
                \k -> { k | shift = on }

            32 ->
                \k -> { k | space = on }

            33 ->
                \k -> { k | pageUp = on }

            34 ->
                \k -> { k | pageDown = on }

            72 ->
                \k -> { k | kH = on }

            74 ->
                \k -> { k | kJ = on }

            75 ->
                \k -> { k | kK = on }

            76 ->
                \k -> { k | kL = on }

            87 ->
                \k -> { k | kW = on }

            65 ->
                \k -> { k | kA = on }

            83 ->
                \k -> { k | kS = on }

            68 ->
                \k -> { k | kD = on }

            37 ->
                \k -> { k | left = on }

            39 ->
                \k -> { k | right = on }

            38 ->
                \k -> { k | up = on }

            40 ->
                \k -> { k | down = on }

            188 ->
                \k -> { k | kComma = on }

            190 ->
                \k -> { k | kPeriod = on }

            73 ->
                \k -> { k | kI = on }

            67 ->
                \k -> { k | kC = on }

            80 ->
                \k -> { k | kP = on }

            _ ->
                Basics.identity
        )
            |> Model.KeyChange


keysToInputs : Model.Keys -> Model.Inputs -> Model.Inputs
keysToInputs keys inputs0 =
    let
        risingEdge old new =
            new && (not old)

        minusPlus v a b =
            if a && not b then
                -v
            else if b && not a then
                v
            else
                0
        unshifted x =
            x && not keys.shift

        shifted x =
            x && keys.shift
    in
        { inputs0
            | x = minusPlus 1.0 keys.kA keys.kD
            , y = minusPlus 1.0 keys.kS keys.kW
            , mx = minusPlus 1.0 keys.left keys.right
            , my = minusPlus 1.0 keys.down keys.up
            , cx = minusPlus 1.0 keys.kH keys.kL
            , cy = minusPlus 1.0 keys.kJ keys.kK
            , button_X = risingEdge inputs0.button_X keys.space
            , prevCamera = shifted <| risingEdge inputs0.prevCamera keys.kC
            , nextCamera = unshifted <| risingEdge inputs0.nextCamera keys.kC
            , toggleOverlay = risingEdge inputs0.toggleOverlay keys.kI
            , prevOverlay = risingEdge inputs0.prevOverlay keys.pageUp
            , nextOverlay = risingEdge inputs0.nextOverlay keys.pageDown
            -- , mx = minusPlus keys.kComma keys.kPeriod
        }


