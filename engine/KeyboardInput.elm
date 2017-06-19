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


