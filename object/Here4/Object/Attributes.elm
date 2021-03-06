module Here4.Object.Attributes
    exposing
        ( VehicleAttributes
        , Action(..)
        , Attributes
        , ObjectAttributes(..)
        , ObjectResult(..)
        , ObjectMsg(..)
        , defaultAttributes
        , id
        , label
        , canFloat
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

import Here4.App exposing (..)
import Here4.App.Types exposing (CtrlMsg)
import Here4.Appearance exposing (Appearance)
import Here4.Body exposing (..)
import Here4.Location exposing (..)
import Here4.Model exposing (Inputs)
import Here4.Orientation as Orientation exposing (Orientation)
import Here4.Setter exposing (..)
import Here4.Object.FlatTexture exposing (..)
import Here4.Object.ReflectiveObj exposing (..)
import Here4.Object.TexturedObj as TexturedObj exposing (..)
import Here4.Vehicle exposing (Driveable)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Html exposing (Html)


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
    , canFloat : Bool
    , position : Vec3
    , scale : Scale
    , offset : Offset
    , rotation : Maybe Orientation
    , overlay : Html (CtrlMsg msg)
    , object : ObjectAttributes
    , action : Action vehicle
    }


type ObjectAttributes
    = Invisible ()
    | Appearance Appearance Vec3
    | FlatTexture FlatTextureAttributes
    | TexturedObj TexturedObjAttributes
    | ReflectiveObj ReflectiveObjAttributes


type ObjectResult
    = InvisibleResult ()
    | FlatTextureResult FlatTextureResult
    | TexturedObjResult TexturedObjResult
    | ReflectiveObjResult ReflectiveObjResult


type ObjectMsg
    = FlatTextureMsg FlatTextureMsg
    | TexturedObjMsg TexturedObjMsg
    | ReflectiveObjMsg ReflectiveObjMsg


defaultAttributes : Attributes vehicle msg
defaultAttributes =
    { id = ""
    , label = ""
    , canFloat = False
    , position = vec3 0 0 0
    , scale = Scale 1.0
    , offset = WorldSpace 0 0 0
    , rotation = Nothing
    , overlay = Html.text ""
    , object = Invisible ()
    , action = Statue
    }


id : String -> Update { a | id : String }
id s attr =
    { attr | id = s }


label : String -> Update { a | label : String }
label l attr =
    { attr | label = l }


canFloat : Bool -> Update { a | canFloat : Bool }
canFloat b attr =
    { attr | canFloat = b }


position : Vec3 -> Update { a | position : Vec3 }
position pos attr =
    { attr | position = pos }


scale : Scale -> Update { a | scale : Scale }
scale s attr =
    { attr | scale = s }


offset : Offset -> Update { a | offset : Offset }
offset off attr =
    { attr | offset = off }


forward : Vec3 -> Update { a | rotation : Maybe Orientation }
forward fwd attr =
    { attr | rotation = Just (Orientation.fromTo V3.k fwd) }


rotation : Orientation -> Update { a | rotation : Maybe Orientation }
rotation o attr =
    { attr | rotation = Just o }


overlay : Html (CtrlMsg msg) -> Update { a | overlay : Html (CtrlMsg msg) }
overlay o attr =
    { attr | overlay = o }


object : ObjectAttributes -> Update { a | object : ObjectAttributes }
object o attr =
    { attr | object = o }


portal : Location -> Update { a | action : Action vehicle }
portal location attr =
    { attr | action = Portal location }


vehicle : VehicleAttributes vehicle -> Update { a | action : Action vehicle }
vehicle v attr =
    { attr | action = Vehicle v }
