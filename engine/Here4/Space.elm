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
import Here4.Navigator.Control exposing (NavMsg)
import Here4.Update as Update
import Here4.View as View
import Html
import Keyboard.Extra
import Mouse
import Time exposing (Time)
import Window


programWithFlags :
    (model -> Sub (NavMsg navMsg))
    -> Methods flags model (NavMsg navMsg)
    -> Program flags (Model.Model model (WorldMsg (NavMsg navMsg))) (Model.Msg (NavMsg navMsg) (WorldMsg (NavMsg navMsg)))
programWithFlags navSubscriptions methods =
    Html.programWithFlags
        { init = Model.init methods.init
        , update = Update.update methods
        , subscriptions = subscriptions navSubscriptions
        , view = View.view methods
        }


subscriptions :
    (worldModel -> Sub (NavMsg navMsg))
    -> Model.Model worldModel worldMsg
    -> Sub (Model.Msg (NavMsg navMsg) (WorldMsg (NavMsg navMsg)))
subscriptions navSubscriptions model =
    [ AnimationFrame.diffs (Model.Animate << Time.inSeconds)
    , Keyboard.Extra.downs (KeyboardInput.keyChange True)
    , Keyboard.Extra.ups (KeyboardInput.keyChange False)
    , Window.resizes Model.Resize
    , Sub.map (Model.WorldMessage << Hub) (navSubscriptions model.multiverse)
    ]
        |> Sub.batch
