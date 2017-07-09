module App exposing (..)

import App.Control exposing (..)
import App.Internal as Internal
import Body exposing (..)
import Camera
import Camera.Types exposing (Framing)
import Dispatch exposing (..)
import Ground
import Location exposing (Location)
import Model exposing (PartyKey)
import Task exposing (Task)
import Time

type alias App = Internal.App
type alias AppMsg = Internal.AppMsg

type alias AppPosition = Internal.AppPosition
type alias Focus = Internal.Focus

type alias Framing = Camera.Types.Framing

type alias Ground = Ground.Ground

type alias PartyKey = Model.PartyKey

type alias Time = Time.Time

create : ( model, Cmd (CtrlMsg msg) ) -> Internal.Animated model (CtrlMsg msg) -> ( App, Cmd AppMsg )
create = Internal.create

createUncontrolled : ( model, Cmd msg ) -> Internal.Animated model msg -> ( App, Cmd AppMsg )
createUncontrolled = Internal.createUncontrolled

toFraming : Moving a -> Framing
toFraming = Camera.toFraming

appToFocus : Oriented a -> Focus
appToFocus = Internal.appToFocus

orientedToFocus : Oriented a -> Focus
orientedToFocus = Internal.orientedToFocus

noFraming : PartyKey -> model -> Maybe Framing
noFraming _ _ =
    Nothing

teleport : PartyKey -> Location -> Cmd (CtrlMsg a)
teleport partyKey location =
    Task.succeed location |> Task.perform (Effect << RelocateParty () partyKey)
