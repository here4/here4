module Navigator exposing (..)

import App
import Color
import Model exposing (Args)

type alias Navigator model msg =
    Program Args (Model.Model model msg) (Model.Msg msg)

type alias App = App.App
type alias AppMsg = App.AppMsg

rgb : Int -> Int -> Int -> Color.Color
rgb = Color.rgb
