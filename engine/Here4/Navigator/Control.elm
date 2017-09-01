module Here4.Navigator.Control exposing (..)

import Here4.Dispatch exposing (..)
import Here4.Model exposing (NavigatorMsg)


type alias NavMsg a =
    Dispatch NavigatorMsg () a
