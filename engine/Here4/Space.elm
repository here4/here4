module Here4.Space exposing (programWithFlags)

{-| This module drives a virtuul wurld


# Program entry

@docs main

-}

import AnimationFrame
import Here4.Control exposing (WorldMsg)
import Here4.Dispatch exposing (..)
import Here4.KeyboardInput as KeyboardInput
import Here4.Methods exposing (..)
import Here4.Model as Model
import Here4.Update as Update
import Here4.View as View
import Html
import Keyboard.Extra
import Mouse
import Ports
import Time exposing (Time)
import Window


programWithFlags :
    Methods flags model msg
    -> Program flags (Model.Model model (WorldMsg msg)) (Model.Msg navMsg (WorldMsg msg))
programWithFlags methods =
    Html.programWithFlags
        { init = Model.init methods.init
        , update = Update.update methods
        , subscriptions = subscriptions
        , view = View.view methods
        }


subscriptions : Model.Model worldModel worldMsg -> Sub (Model.Msg navMsg worldMsg)
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
           )
-}
        |> Sub.batch
