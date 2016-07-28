module App exposing (programWithFlags)

{-| This module drives a virtuul wurld

# Program entry
@docs main
-}

import AnimationFrame
import Html.App as Html
import Keyboard
import Mouse
import Time exposing (Time)
import Window

import Ports

import Model
import Update
import View

import Things.Terrain exposing (Terrain)

programWithFlags
  : { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Maybe Model.World
    , animate : Time -> model -> model 
    , terrain : model -> Maybe Terrain
    }
  -> Program Model.Args
programWithFlags world =
    Html.programWithFlags
        { init = Model.init world.init
        , update = Update.update world.update world.terrain world.animate
        , subscriptions = subscriptions
        , view = View.view world.view
        }

{- Subscribe to keychange events.
Ignore anything that isn't an escape, space or WASD keys.
-}
keyChange : Bool -> Keyboard.KeyCode -> Model.Msg worldMsg
keyChange on keyCode =
    if keyCode == 27 && on then
        Model.LockRequest False
    else
        (case keyCode of
            32 -> \k -> { k | space = on }
            65 -> \k -> { k | left  = on }
            68 -> \k -> { k | right = on }
            87 -> \k -> { k | up    = on }
            83 -> \k -> { k | down  = on }
            _  -> Basics.identity
        ) |> Model.KeyChange

subscriptions : Model.Model worldModel -> Sub (Model.Msg worldMsg)
subscriptions model =
    [ AnimationFrame.diffs (Model.Animate << Time.inSeconds)
    , Keyboard.downs (keyChange True)
    , Keyboard.ups (keyChange False)
    , Window.resizes Model.Resize
    , Ports.isLocked Model.LockUpdate
    ]
        ++ (if model.isLocked then
                [ Ports.movement Model.MouseMove ]
            else
                [ Mouse.clicks (\_ -> Model.LockRequest True) ]
           )
        |> Sub.batch

{-
-- import Automaton
import Char exposing (toCode)
import Graphics.Element exposing (..)
import Http
import Maybe.Extra exposing (mapDefault)
import Random
import Set
import Signal exposing (dropRepeats, sampleOn, merge)
import Signal.Extra exposing (combine)
import String exposing (contains)
import Task exposing (Task, andThen)
import Text
import Time exposing (Time, fps)

import WebGL exposing (..)
import Math.Matrix4 exposing (..)
import Keyboard
import Mouse
import Window

import Array2D exposing (Array2D)
import Gamepad
import GamepadInputs exposing (gamepadInputs, persistentGamepads)
import LoadObj exposing (objMailbox, sendRaw, objJeepMailbox)
import Math.Procedural exposing (..)
import Model exposing (noInput)
import Engine exposing (..)
import Update
import Things.Surface2D exposing (Placement, defaultPlacement)

import Demo

import Color exposing (black, white)
import FontAwesome
import Html exposing (toElement)

import Graphics.Collage exposing (collage, defaultLine, outlinedText, text)

import Debug
import Orientation

-- Pointer Lock information
port movement : Signal (Int,Int)
port isLocked : Signal Bool

-- Ability to request and exit. Click screen to request lock. Press escape to
-- give up the lock. This code can all be removed if you want to do this
-- differently.

port requestPointerLock : Signal ()
port requestPointerLock =
    let dropWhen bs def sig = Signal.map snd
            (Signal.filter fst (False, def) (Signal.map2 (,) (Signal.sampleOn sig (Signal.map not bs)) sig))
    in
    dropWhen (Signal.map2 (&&) Keyboard.shift isLocked) () Mouse.clicks


port exitPointerLock : Signal ()
port exitPointerLock =
    Signal.map (always ()) (Signal.filter (Set.member 27) Set.empty Keyboard.keysDown)

----------------------------------------------------------------------
-- Resource loading

port fetchObj : Task Http.Error ()
port fetchObj =
    Http.getString "resources/wt_teapot.obj" `andThen` sendRaw objMailbox

port fetchJeep : Task Http.Error ()
port fetchJeep =
    Http.getString "resources/Jeep.obj" `andThen` sendRaw objJeepMailbox

----------------------------------------------------------------------
-- Inputs

mouseDeltas : Signal (Time, (Float, Float))
mouseDeltas =
    let step (t,(mx,my)) pt = ((Time.inSeconds t - pt, (toFloat mx, toFloat my)), t)
        a = Automaton.hiddenState 0 step
    in Automaton.run a (0, (0,0)) (Time.timestamp movement)

-- Set up 3D world
kbMouseInputs : Signal Model.Inputs
kbMouseInputs =
  let dt = Signal.map Time.inSeconds (fps 60)
      dirKeys = merge Keyboard.arrows Keyboard.wasd
      yo x = x / 500
      handleKeys s bx {x,y} kdt = dbg_X
          { noInput | isJumping = s, button_X = bx, x = (toFloat x)/1000, y = (toFloat y)*1000, dt=kdt }
      dbg_X i = if i.button_X then Debug.log "Button X pressed" i else i
      x_key = Keyboard.isDown 88
  in  merge
          (sampleOn dt <| Signal.map4 handleKeys Keyboard.space x_key dirKeys dt)
          (Signal.map (\(mt, (mx,my)) -> { noInput | mx= yo mx, my= yo my, mt = mt }) mouseDeltas)

atWithDefault : a -> Int -> List a -> a
atWithDefault def n list = Maybe.withDefault def (List.head (List.drop n list))

gamepad1 : Signal Model.Inputs
gamepad1 = Signal.map (atWithDefault noInput 0) gamepadInputs

gamepad2 : Signal Model.Inputs
gamepad2 = Signal.map (atWithDefault noInput 1) gamepadInputs

-- inputs : Signal Model.Inputs
-- inputs = merge (dropRepeats kbMouseInputs) (dropRepeats gamepad1)

inputs1 : Signal Model.Inputs
inputs1 = merge (dropRepeats kbMouseInputs) (dropRepeats gamepad1)

inputs2 : Signal Model.Inputs
inputs2 = dropRepeats gamepad2

-- person : Placement -> Array2D Float -> Signal Model.Person
-- person placement terrain = Signal.foldp (Update.step placement terrain) Model.defaultPerson inputs

person1 : Placement -> Array2D Float -> Signal Model.Person
person1 placement terrain = Signal.foldp (Update.step placement terrain) Model.defaultPerson inputs1

person2 : Placement -> Array2D Float -> Signal Model.Person
person2 placement terrain = Signal.foldp (Update.step placement terrain) Model.defaultPerson inputs2

{-| The main function -}
main : Signal Element
main = world Demo.demoThings

world : (Array2D Float -> List (Signal Model.Person) -> Signal (List Thing)) -> Signal Element
world thingsOnTerrain =
  let fs = Signal.map Time.inSeconds (fps 60)
      t = Signal.foldp (+) 0 fs
      ma alpha new old = alpha * old + (1-alpha)*new
      measuredFPS = sampleOn (fps 3)
          <| Signal.map (\dt -> 1.0/dt)
          <| Signal.foldp (ma 0.9) (Time.inSeconds (1.0/15)) fs
      wh  = Window.dimensions
      wh2 = Signal.map (\(w,h) -> (w//2, h)) wh

      placement = defaultPlacement

      seed0 = Random.initialSeed 7777
      (terrain, seed1) = Random.generate (randTerrain2D (placement.bigSide+1)) seed0

      person1' = person1 placement terrain
      person2' = person2 placement terrain
      entities = thingsOnTerrain terrain [person1', person2']

      oneScene = Signal.map5 (scene Model.OneEye) entities wh t measuredFPS person1'
      dualScene =
            (Signal.map2 beside
                (Signal.map5 (scene Model.OneEye) entities wh2 t measuredFPS person1')
                (Signal.map5 (scene Model.OneEye) entities wh2 t measuredFPS person2'))
      vrScene =
            (Signal.map2 beside
                (Signal.map5 (scene Model.LeftEye) entities wh2 t measuredFPS person1')
                (Signal.map5 (scene Model.RightEye) entities wh2 t measuredFPS person1'))

      ifElse : (a -> Bool) -> b -> b -> a -> b
      ifElse p ifBranch elseBranch x = if p x then ifBranch else elseBranch
      oneOrTwoPlayerScene = Signal.map3 (ifElse (\l -> List.length l > 1)) dualScene oneScene persistentGamepads
      -- Signal.map3 lockMessage wh isLocked
      info2DView = Signal.map2 debugLayer
          --(combine [Signal.map show Gamepad.gamepads, Signal.map show gamepadInputs])
          -- (combine [Signal.map (show << mapTriple (round << toDegrees)) (Signal.map (Qn.toEuler << .orientQn) person1')])
            (Signal.map infoLayer person1')
            oneOrTwoPlayerScene
  in
      -- If person1' is in VR mode then just show that, else the 2D view with infolayer
      Signal.map3 (ifElse (\p -> p.cameraVR)) vrScene info2DView person1'

infoLayer : Model.Person -> Int -> Element
infoLayer person w = container w 84 middle <| flow right <|
    [ vehicleInfo person
    , flow right [bigIcon FontAwesome.diamond, bigShow 7]
    , bigShow <| mapTriple (round << toDegrees) (Orientation.toRollPitchYaw person.orientation)
    ]

vehicleInfo : Model.Person -> Element
vehicleInfo person =
    let
        vehicleName = if person.vehicle == Model.vehicleBird then
                          "Dreambird"
                      else if person.vehicle == Model.vehicleBuggy then
                           "Dreambuggy"
                      else "DreamDebug"
        -- vehicleIcon = if person.flying then FontAwesome.plane else FontAwesome.car
        wher = if person.cameraInside then "Inside" else "Outside"
    in
        flow right <|
            [-- bigIcon vehicleIcon, spacer 48 72,
            bigText vehicleName
            ,bigText wher
            ]

bigIcon : (Color.Color -> Int -> Html.Html) -> Element
bigIcon icon = container 48 72 middle <| Html.toElement 48 48 (icon white 48)

textElement : Int -> Int -> Text.Text -> Element
textElement w h t = container w 72 middle <| collage w h [text t, outlinedText defaultLine t]

bigShow = toString >> bigText

bigText = Text.fromString
    >> Text.height 48
    >> Text.typeface ["helvetica","arial","sans-serif"]
    >> Text.color white
    >> textElement 300 72

mapTriple : (a -> b) -> (a,a,a) -> (b,b,b)
mapTriple f (x,y,z) = (f x, f y, f z)

toDegrees : Float -> Float
toDegrees rad = 360 * rad / (2*pi)

debugLayer : (Int -> Element) -> Element -> Element
debugLayer dbgF e =
    let
        dbg = dbgF (widthOf e)
        r = spacer (widthOf e) (heightOf dbg)
            |> color black
            |> opacity 0.3
    in
        layers [ e, r, dbg ]

lockMessage : (Int,Int) -> Bool -> Element -> Element
lockMessage (w,h) isLocked e =
    layers [ e
           , container w 140 (midLeftAt (absolute 40) (relative 0.5))
                 (if isLocked then exitMsg else enterMsg)
           ]

enterMsg : Element
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : Element
exitMsg = message "Press <escape> to exit full screen."

message : String -> Element
message msg =
   leftAligned <| Text.monospace <| Text.fromString <|
    "Use gamepad, arrows or WASD keys to move.\n\n" ++ msg
-}
