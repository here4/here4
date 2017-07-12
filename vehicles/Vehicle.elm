module Vehicle exposing (..)

type alias Driveable a =
    { a | speed : Float
        , height : Float
        , radius : Float
    }
