module Object.Attributes exposing
    ( VehicleAttributes
    , Action(..)
    , Attributes
    , defaultAttributes
    , id
    , label
    , position
    , scale
    , offset
    , forward
    , rotation
    , overlay
    , object
    , portal
    , vehicle
    )

import App exposing (..)
import App.Control exposing (CtrlMsg)
import Body exposing (..)
import Html exposing (Html)
import Location exposing (..)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Model exposing (Inputs)
import Object exposing (ObjectAttributes(..))
import Orientation exposing (Orientation)
import Setter exposing (..)
import Vehicle exposing (Driveable)

type alias VehicleAttributes vehicle =
    { drive : Driveable vehicle -> Vec3 -> Ground -> Inputs -> Moving {} -> Moving {}
    , vehicle : Driveable vehicle
    }

type Action vehicle
    = Statue
    | Portal Location
    | Vehicle (VehicleAttributes vehicle)

type alias Attributes vehicle msg =
    { id : String
    , label : String
    , position : Vec3
    , scale : Vec3
    , offset : Vec3
    , rotation : Maybe Orientation
    , overlay : Html (CtrlMsg msg)
    , object : ObjectAttributes
    , action : Action vehicle
    }


defaultAttributes : Attributes vehicle msg
defaultAttributes =
    { id = ""
    , label = ""
    , position = vec3 0 0 0
    , scale = vec3 1 1 1
    , offset = vec3 0 0 0
    , rotation = Nothing
    , overlay = Html.text ""
    , object = Invisible ()
    , action = Statue
    }


id : String -> Update { a | id : String }
id s attr = { attr | id = s }

label : String -> Update { a | label : String }
label l attr = { attr | label = l }

position : Vec3 -> Update { a | position : Vec3 }
position pos attr = { attr | position = pos }

scale : Vec3 -> Update { a | scale : Vec3 }
scale s attr = { attr | scale = s }

offset : Vec3 -> Update { a | offset : Vec3 }
offset off attr = { attr | offset = off }

forward : Vec3 -> Update { a | rotation : Maybe Orientation }
forward fwd attr = { attr | rotation = Just (Orientation.fromTo V3.k fwd) }

rotation : Orientation -> Update { a | rotation : Maybe Orientation }
rotation o attr = { attr | rotation = Just o }

overlay : Html (CtrlMsg msg) -> Update { a | overlay : Html (CtrlMsg msg) }
overlay o attr = { attr | overlay = o }

object : ObjectAttributes -> Update { a | object : ObjectAttributes }
object o attr = { attr | object = o }

portal : Location -> Update { a | action : Action vehicle }
portal location attr = { attr | action = Portal location }

vehicle : VehicleAttributes vehicle -> Update { a | action : Action vehicle }
vehicle v attr = { attr | action = Vehicle v }

