module Here4.KeyboardInput exposing (..)

import Here4.Model as Model
import Keyboard.Extra exposing (Key(..))


{- Subscribe to keychange events. -}


keyChange : Bool -> Key -> Model.Msg worldMsg
keyChange on key =
    {-
       if key == Escape && on then
           Model.LockRequest False
       else
    -}
    (case key of
        Tab ->
            \k -> { k | tab = on }

        Shift ->
            \k -> { k | shift = on }

        Space ->
            \k -> { k | space = on }

        PageUp ->
            \k -> { k | pageUp = on }

        PageDown ->
            \k -> { k | pageDown = on }

        CharH ->
            \k -> { k | kH = on }

        CharJ ->
            \k -> { k | kJ = on }

        CharK ->
            \k -> { k | kK = on }

        CharL ->
            \k -> { k | kL = on }

        CharW ->
            \k -> { k | kW = on }

        CharA ->
            \k -> { k | kA = on }

        CharS ->
            \k -> { k | kS = on }

        CharD ->
            \k -> { k | kD = on }

        ArrowLeft ->
            \k -> { k | left = on }

        ArrowRight ->
            \k -> { k | right = on }

        ArrowUp ->
            \k -> { k | up = on }

        ArrowDown ->
            \k -> { k | down = on }

        Comma ->
            \k -> { k | kComma = on }

        Period ->
            \k -> { k | kPeriod = on }

        CharI ->
            \k -> { k | kI = on }

        CharC ->
            \k -> { k | kC = on }

        CharP ->
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
            | x = inputs0.x + minusPlus 0.5 keys.kA keys.kD
            , y = inputs0.y + minusPlus 1.0 keys.kS keys.kW
            , mx = inputs0.mx + minusPlus 1.0 keys.left keys.right
            , my = inputs0.my + minusPlus 1.0 keys.down keys.up
            , cx = inputs0.cx + minusPlus 1.0 keys.kH keys.kL
            , cy = inputs0.cy + minusPlus 1.0 keys.kJ keys.kK
            , button_Y = risingEdge inputs0.button_X keys.space
            , prevCamera = shifted <| risingEdge inputs0.prevCamera keys.kC
            , nextCamera = unshifted <| risingEdge inputs0.nextCamera keys.kC
            , toggleOverlay = risingEdge inputs0.toggleOverlay keys.kI
            , prevOverlay = risingEdge inputs0.prevOverlay keys.pageUp
            , nextOverlay = risingEdge inputs0.nextOverlay keys.pageDown

            -- , mx = minusPlus keys.kComma keys.kPeriod
        }
