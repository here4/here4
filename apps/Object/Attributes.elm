module Object.Attributes exposing
    ( VehicleAttributes
    , Action(..)
    , Attributes
    , Update
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
import Vehicle exposing (Driveable)

type alias VehicleAttributes vehicle =
    { drive : Driveable vehicle -> Ground -> Inputs -> Moving {} -> Moving {}
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
    , scale : Float
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
    , scale = 1.0
    , offset = vec3 0 0 0
    , rotation = Nothing
    , overlay = Html.text ""
    , object = Invisible ()
    , action = Statue
    }


type alias Update vehicle msg = 
    Attributes vehicle msg -> Attributes vehicle msg

id : String -> Update vehicle msg
id s attr = { attr | id = s }

label : String -> Update vehicle msg
label l attr = { attr | label = l }

position : Vec3 -> Update vehicle msg
position pos attr = { attr | position = pos }

scale : Float -> Update vehicle msg
scale s attr = { attr | scale = s }

offset : Vec3 -> Update vehicle msg
offset off attr = { attr | offset = off }

forward : Vec3 -> Update vehicle msg
forward fwd attr = { attr | rotation = Just (Orientation.fromTo fwd V3.k) }

rotation : Orientation -> Update vehicle msg
rotation o attr = { attr | rotation = Just o }

overlay : Html (CtrlMsg msg) -> Update vehicle msg
overlay o attr = { attr | overlay = o }

object : ObjectAttributes -> Update vehicle msg
object o attr = { attr | object = o }

portal : Location -> Update vehicle msg
portal location attr = { attr | action = Portal location }

vehicle : VehicleAttributes vehicle -> Update vehicle msg
vehicle v attr = { attr | action = Vehicle v }

