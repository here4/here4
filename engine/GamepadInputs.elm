module GamepadInputs exposing (GamepadButtons, persistentGamepads, gamepadToArrows, gamepadToButtons)

-- import Maybe.Extra exposing (mapDefault)

import String exposing (contains)
import Gamepad exposing (..)


----------------------------------------------------------------------
-- Gamepad


gamepadToArrows : Gamepad.Gamepad -> { x : Float, y : Float, mx : Float, my : Float, cx : Float, cy : Float }
gamepadToArrows gamepad =
    case gamepad of
        StandardGamepad g ->
            let
                sgn x =
                    if x < 0 then
                        -1
                    else
                        1

                -- Apply a deadzone of size l around an axis centered at 0
                u l x =
                    if abs x < l then
                        0.0
                    else
                        sgn x * (abs x - l) / (1.0 - l)

                deadzone =
                    u 0.2

                {-
                   -- Map a trigger to a standard range [0..1], applying a deadzone
                   dc l x = let xm = (x+1.0)/2.0 in u l xm
                   toTrigger = dc 0.2
                -}
                axs =
                    { x = deadzone g.leftStick.x / 10
                    , y = deadzone (-1.0 * g.leftStick.y) / 10
                    , mx = deadzone g.rightStick.x / 20
                    , my = deadzone (-1.0 * g.rightStick.y) / 20
                    , cx = 0
                    , cy = 0
                    }

                -- Interpret brake, accelerator as y input
                ( btns_y, cx, cy ) =
                    ( g.rightTrigger.value - g.leftTrigger.value
                    , g.dPadRight.value - g.dPadLeft.value
                    , g.dPadUp.value - g.dPadDown.value
                    )
            in
                { axs | y = btns_y + axs.y, cx = cx, cy = cy }

        RawGamepad g ->
            { x = 0, y = 0, mx = 0, my = 0, cx = 0, cy = 0 }


gamepadsToArrows : List Gamepad.Gamepad -> List { x : Float, y : Float, mx : Float, my : Float, cx : Float, cy : Float }
gamepadsToArrows =
    List.map gamepadToArrows



{-
   gamepadNothing : Gamepad.Gamepad
   gamepadNothing =
       { id = "", axes = [], buttons = [], mapping = "" }

   toStandardGamepad : Gamepad.Gamepad -> Gamepad.Gamepad
   toStandardGamepad gamepad =
       if contains gamepad.id "STANDARD GAMEPAD" then
           gamepad
       else
           let
               ( axes_, buttons_ ) =
                   case ( gamepad.axes, gamepad.buttons ) of
                       -- Performance Designed Products Rock Candy Gamepad for Xbox 360 (Vendor: 0e6f Product: 011f)
                       ( [ x1, y1, b1, x2, y2, b2, x3, y3 ], [ a, b, x, y, lb, rb, back, start, logo, lstick, rstick ] ) ->
                           let
                               toTrigger b =
                                   let
                                       b_ =
                                           (b + 1.0) / 2.0
                                   in
                                       if b_ > 0 then
                                           { pressed = True, value = b_ }
                                       else
                                           { pressed = False, value = 0 }

                               b1_ =
                                   toTrigger b1

                               b2_ =
                                   toTrigger b2

                               toPads ax =
                                   if ax > 0 then
                                       ( { pressed = True, value = ax }, { pressed = False, value = 0 } )
                                   else if ax < 0 then
                                       ( { pressed = False, value = 0 }, { pressed = True, value = -ax } )
                                   else
                                       ( { pressed = False, value = 0 }, { pressed = False, value = 0 } )

                               ( padR, padL ) =
                                   toPads x3

                               ( padD, padU ) =
                                   toPads y3
                           in
                               ( [ x1, y1, x2, y2 ]
                               , [ a
                                 , b
                                 , x
                                 , y
                                 , lb
                                 , rb
                                 , b1_
                                 , b2_
                                 , back
                                 , start
                                 , lstick
                                 , rstick
                                 , padU
                                 , padD
                                 , padL
                                 , padR
                                 , logo
                                 ]
                               )

                       _ ->
                           ( gamepad.axes, gamepad.buttons )
           in
               { gamepad | axes = axes_, buttons = buttons_ }
-}


persistentGamepads : List String -> List Gamepad.Gamepad -> ( List (Maybe Gamepad.Gamepad), List String )
persistentGamepads is0 gs0 =
    let
        gamepadId g =
            case g of
                StandardGamepad g_ ->
                    g_.id

                RawGamepad g_ ->
                    g_.id

        extract acc i gs0 =
            case gs0 of
                [] ->
                    ( Nothing, List.reverse acc ++ gs0 )

                g :: gs ->
                    if gamepadId g == i then
                        ( Just g, List.reverse acc ++ gs )
                    else
                        extract (g :: acc) i gs

        reorder is0 gs0 =
            case is0 of
                [] ->
                    List.map Just (List.sortBy gamepadId gs0)

                i :: is ->
                    let
                        ( gm, gs ) =
                            extract [] i gs0
                    in
                        gm :: reorder is gs

        catMaybes =
            List.filterMap Basics.identity

        remap : List String -> List (Maybe Gamepad.Gamepad) -> List String
        remap ids0 gs0 =
            case ( ids0, gs0 ) of
                ( [], _ ) ->
                    catMaybes (List.map (Maybe.map gamepadId) gs0)

                ( is, [] ) ->
                    is

                ( i :: is, g :: gs ) ->
                    Maybe.withDefault i (Maybe.map gamepadId g) :: remap is gs

        gs =
            reorder is0 gs0
    in
        ( gs, remap is0 gs )



{-
   standardGamepads : Signal (List Gamepad.Gamepad)
   standardGamepads = Signal.map (List.map toStandardGamepad) Gamepad.gamepads

   persistentGamepads : Signal (List (Maybe Gamepad.Gamepad))
   persistentGamepads =
       let
           extract acc i gs0 = case gs0 of
               []      -> (Nothing, List.reverse acc ++ gs0)
               (g::gs) -> if g.id == i then (Just g, List.reverse acc ++ gs)
                          else extract (g::acc) i gs

           reorder is0 gs0 = case is0 of
               []        -> List.map Just (List.sortBy getId gs0)
               (i::is) -> let (gm,gs) = extract [] i gs0 in gm :: reorder is gs

           getId g = g.id
           catMaybes = List.filterMap Basics.identity

           remap : List String -> List (Maybe Gamepad.Gamepad) -> List String
           remap ids0 gs0 = case (ids0, gs0) of
               ([], _)            -> catMaybes (List.map (Maybe.map getId) gs0)
               (is, [])           -> is
               ((i::is), (g::gs)) -> Maybe.withDefault i (Maybe.map getId g) :: remap is gs

           step gs0 is0 = let gs = reorder is0 gs0 in (gs, remap is0 gs)
           a = Automaton.hiddenState [] step
       in Automaton.run a [] standardGamepads
-}
{- a b x y lbumper rbumper l r
   l tiny (back), r tiny (start)
   stick l down, stick r down
   4way: u d l r
   Xbox-logo
-}


type alias GamepadButtons =
    { bA : Bool
    , bB : Bool
    , bX : Bool
    , bY : Bool
    , bLeftBumper : Bool
    , bRightBumper : Bool
    , bBack : Bool
    , bStart : Bool
    }


gamepadButtonsNone : GamepadButtons
gamepadButtonsNone =
    { bA = False
    , bB = False
    , bX = False
    , bY = False
    , bLeftBumper = False
    , bRightBumper = False
    , bBack = False
    , bStart = False
    }


gamepadToButtons : Gamepad.Gamepad -> GamepadButtons
gamepadToButtons gamepad =
    case gamepad of
        StandardGamepad g ->
            { bA = g.buttonA.pressed
            , bB = g.buttonB.pressed
            , bX = g.buttonX.pressed
            , bY = g.buttonY.pressed
            , bLeftBumper = g.leftBumper.pressed
            , bRightBumper = g.rightBumper.pressed
            , bBack = g.buttonBack.pressed
            , bStart = g.buttonStart.pressed
            }

        _ ->
            gamepadButtonsNone


gamepadsToButtons : List Gamepad.Gamepad -> List GamepadButtons
gamepadsToButtons =
    List.map gamepadToButtons



----------------------------------------------------------------------
{-
   gamepadToInputs : Time -> Gamepad.Gamepad -> Model.Inputs
   gamepadToInputs dt gamepad =
       let {x,y,mx,my} = gamepadToArrows gamepad
           bs = gamepadToButtons gamepad
       in  { noInput | reset = bs.bStart, changeVR = bs.bB, changeCamera = bs.bRightBumper, x = x, y = y, mx=mx, my=my, button_X = bs.bX, dt = dt }

   gamepadsToInputs : List (Maybe Gamepad.Gamepad) -> Time -> List Model.Inputs
   gamepadsToInputs gamepads dt = List.map (mapDefault noInput (gamepadToInputs dt)) gamepads

   gamepadInputs : Signal (List Model.Inputs)
   gamepadInputs =
     let dt = Signal.map Time.inSeconds (fps 60)
     in  sampleOn dt <| Signal.map2 gamepadsToInputs persistentGamepads dt

-}
