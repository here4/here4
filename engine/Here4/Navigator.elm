module Here4.Navigator exposing (..)

import Color
import Here4.App as App
import Here4.Model as Model


type alias Navigator flags model navMsg msg =
    Program flags (Model.Model model msg) (Model.Msg navMsg msg)


type alias App =
    App.App


type alias AppMsg =
    App.AppMsg


rgb : Int -> Int -> Int -> Color.Color
rgb =
    Color.rgb
