module Here4.Navigator exposing (..)

import Color
import Here4.App as App
import Here4.Model as Model
import Here4.Navigator.Control exposing (NavMsg)


type alias Navigator flags model msg =
    Program
        flags
        (Model.Model model msg)
        (Model.Msg (NavMsg msg) msg)


type alias App =
    App.App


type alias AppMsg =
    App.AppMsg


rgb : Int -> Int -> Int -> Color.Color
rgb =
    Color.rgb
