module Navigator exposing (..)

import Model exposing (Args)

type alias Navigator model msg =
    Program Args (Model.Model model msg) (Model.Msg msg)
