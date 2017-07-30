module Here4.Vehicle exposing (..)


type alias Driveable a =
    { a
        | speed : Float
        , height : Float
        , radius : Float
    }
