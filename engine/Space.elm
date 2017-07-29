module Space exposing (programWithFlags)

{-| This module drives a virtuul wurld


# Program entry

@docs main

-}

import AnimationFrame
import Html
import Keyboard.Extra
import KeyboardInput
import Mouse
import Time exposing (Time)
import Window
-- import Ports
import Control exposing (WorldMsg)
import Dispatch exposing (..)
import Methods exposing (..)
import Model
import Update
import View


programWithFlags :
    Methods model msg
    -> Program Model.Args (Model.Model model (WorldMsg msg)) (Model.Msg (WorldMsg msg))
programWithFlags methods =
    Html.programWithFlags
        { init = Model.init methods.init
        , update = Update.update methods
        , subscriptions = subscriptions
        , view = View.view methods
        }


subscriptions : Model.Model worldModel worldMsg -> Sub (Model.Msg worldMsg)
subscriptions model =
    [ AnimationFrame.diffs (Model.Animate << Time.inSeconds)
    , Keyboard.Extra.downs (KeyboardInput.keyChange True)
    , Keyboard.Extra.ups (KeyboardInput.keyChange False)
    , Window.resizes Model.Resize
    -- , Ports.isLocked Model.LockUpdate
    ]
{-
        ++ (if model.isLocked then
                [ Ports.movement Model.MouseMove ]
            else
                [ Mouse.clicks (\_ -> Model.LockRequest True) ]
-}
           )
        |> Sub.batch
