module GamepadInputs exposing (GamepadButtons, persistentGamepads, toStandardGamepad, gamepadToArrows, gamepadToButtons)

-- import Maybe.Extra exposing (mapDefault)
import String exposing (contains)

import Gamepad

----------------------------------------------------------------------
-- Gamepad

gamepadToArrows : Gamepad.Gamepad -> { x : Float, y : Float, mx : Float, my : Float, cx : Float, cy : Float }
gamepadToArrows gamepad =
    let
        sgn x = if x < 0 then -1 else 1
        -- Apply a deadzone of size l around an axis centered at 0
        u l x = if abs x < l then 0.0 else sgn x * (abs x - l) / (1.0 - l)
        deadzone = u 0.2

{-
        -- Map a trigger to a standard range [0..1], applying a deadzone
        dc l x = let xm = (x+1.0)/2.0 in u l xm
        toTrigger = dc 0.2
-}

        axs = case gamepad.axes of
{-
                  -- Performance Designed Products Rock Candy Gamepad for Xbox 360 (Vendor: 0e6f Product: 011f)
                  [x1, y1, b1, x2, y2, b2, x3, y3] ->
                      { x = deadzone x1 / 10
                      , y = deadzone (-1.0 * y1) / 10 - toTrigger b1 + toTrigger b2
                      , mx = deadzone x2 /20
                      , my = deadzone (-1.0 * y2) / 20
                      }
-}

                  -- ©Microsoft Corporation Controller (STANDARD GAMEPAD Vendor: 045e Product: 028e)
                  [x1, y1, x2, y2] ->
                      { x = deadzone x1 / 10
                      , y = deadzone (-1.0 * y1) / 10
                      , mx = deadzone x2 / 20
                      , my = deadzone (-1.0 * y2) / 20
                      , cx = 0
                      , cy = 0
                      }

                  [x1,y1] ->
                      { x = x1/10, y = (-1.0 * y1)/10, mx = 0, my = 0, cx = 0, cy = 0 }

                  _ ->
                      {x = 0, y = 0, mx = 0, my = 0, cx = 0, cy = 0 }

        -- Interpret brake, accelerator as y input
        (btns_y, cx, cy) = case gamepad.buttons of
                    -- ©Microsoft Corporation Controller (STANDARD GAMEPAD Vendor: 045e Product: 028e)
                    [a, b, x, y, lb, rb, l, r, back, start, lstick, rstick, padU, padD, padL, padR, logo]
                        -> ( r.value - l.value, padR.value - padL.value, padU.value - padD.value )

{-
                    -- Performance Designed Products Rock Candy Gamepad for Xbox 360 (Vendor: 0e6f Product: 011f)
                    [a, b, x, y, lt, rt, back, start, logo, lstick, rstick] -> 0
-}

                    _ -> ( 0, 0, 0 )

    in
        { axs | y = btns_y + axs.y, cx = cx, cy = cy }

gamepadsToArrows : List Gamepad.Gamepad -> List { x : Float, y : Float, mx : Float, my : Float, cx : Float, cy : Float }
gamepadsToArrows = List.map gamepadToArrows

gamepadNothing : Gamepad.Gamepad
gamepadNothing = { id = "", axes = [], buttons = [], mapping = "" }

toStandardGamepad : Gamepad.Gamepad -> Gamepad.Gamepad
toStandardGamepad gamepad =
    if gamepad.id `contains` "STANDARD GAMEPAD" then
        gamepad
    else
        let (axes', buttons') = case (gamepad.axes, gamepad.buttons) of
            -- Performance Designed Products Rock Candy Gamepad for Xbox 360 (Vendor: 0e6f Product: 011f)
            ( [x1, y1, b1, x2, y2, b2, x3, y3]
            , [a, b, x, y, lb, rb, back, start, logo, lstick, rstick]) ->
                let
                    toTrigger b = let b' = (b+1.0)/2.0 in
                        if b' > 0 then
                            { pressed = True, value = b' }
                        else
                            { pressed = False, value = 0 }
                    b1' = toTrigger b1
                    b2' = toTrigger b2

                    toPads ax =
                        if ax > 0 then
                            ({ pressed = True, value = ax }, { pressed = False, value = 0 })
                        else if ax < 0 then
                            ({ pressed = False, value = 0 }, { pressed = True, value = -ax })
                        else
                            ({ pressed = False, value = 0 }, { pressed = False, value = 0 })
                    (padR, padL) = toPads x3
                    (padD, padU) = toPads y3

                in
                    ( [x1, y1, x2, y2]
                    , [ a, b, x, y, lb, rb, b1', b2', back, start
                      , lstick, rstick, padU, padD, padL, padR, logo]
                    )

            _ -> (gamepad.axes, gamepad.buttons)
        in { gamepad | axes = axes', buttons = buttons' }

persistentGamepads : List String -> List Gamepad.Gamepad -> (List (Maybe Gamepad.Gamepad), List String)
persistentGamepads is0 gs0 = 
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

        gs = reorder is0 gs0
    in (gs, remap is0 gs)

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
    { bA : Bool, bB : Bool, bX : Bool, bY : Bool
    , bLeftBumper : Bool, bRightBumper : Bool
    , bBack : Bool, bStart : Bool }

gamepadButtonsNone : GamepadButtons
gamepadButtonsNone =
    { bA = False, bB = False, bX = False, bY = False
    , bLeftBumper = False, bRightBumper = False
    , bBack = False, bStart = False }

gamepadToButtons : Gamepad.Gamepad -> GamepadButtons
gamepadToButtons gamepad =
    case gamepad.buttons of
        (a::b::x::y::lb::rb::l::r::back::start::_) ->
            { bA = a.pressed, bB = b.pressed, bX = x.pressed, bY = y.pressed
            , bLeftBumper = lb.pressed, bRightBumper = rb.pressed
            , bBack = back.pressed, bStart = start.pressed }

        _ -> gamepadButtonsNone

gamepadsToButtons : List Gamepad.Gamepad -> List GamepadButtons
gamepadsToButtons = List.map gamepadToButtons

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
