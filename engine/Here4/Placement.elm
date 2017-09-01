module Here4.Placement
    exposing
        ( Placement
        , defaultPlacement
        , coordRangeX
        , coordRangeZ
        )


type alias Placement =
    { xOffset : Float
    , xDelta : Float
    , yOffset : Float
    , yMult : Float
    , zOffset : Float
    , zDelta : Float
    , tileSize : Int -- Length of side of a square tile
    , bigSide : Int
    }


defaultPlacement : Placement
defaultPlacement =
    { xOffset = -256
    , xDelta = 4
    , yOffset = 0
    , yMult = 120
    , zOffset = -256
    , zDelta = 4
    , tileSize = 32
    , bigSide = 512
    }

coordRangeX : Placement -> (Float, Float)
coordRangeX placement =
    (placement.xOffset, placement.xOffset + toFloat placement.bigSide * placement.xDelta)

coordRangeZ : Placement -> (Float, Float)
coordRangeZ placement =
    (placement.zOffset, placement.zOffset + toFloat placement.bigSide * placement.zDelta)
