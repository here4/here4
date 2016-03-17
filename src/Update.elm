module Update (step) where

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Quaternion as Qn
import Util exposing (v3_clamp)

import Array2D exposing (Array2D)
import Model
import Things.Surface2D exposing (Placement)
import Things.Terrain as Terrain
import Vehicles.DreamBird as DreamBird
import Vehicles.DreamBuggy as DreamBuggy

import Debug

step : Placement -> Array2D Float -> Model.Inputs -> Model.Person -> Model.Person
step placement terrain inputs person0 = if inputs.reset then Model.defaultPerson else
        let 
            eyeLevel pos = Model.eyeLevel + Terrain.elevation placement terrain pos
            move person =
                if person.flying then
                      DreamBird.move eyeLevel inputs person
                else
                      DreamBuggy.move eyeLevel inputs person
            bounds person = { person | pos = Terrain.bounds placement person.pos }

            checkCamera person = { person | cameraInside =
                if inputs.changeCamera then not person.cameraInside else person.cameraInside }

            moveCamera person =
                if person.cameraInside then
                    { person | cameraPos = person.pos `add`
                                           (V3.scale 1.5 (Model.direction person))
                             , cameraUp = Model.cameraUp person }
                else
                    let newCameraPos =
                            vec3 0 7 0 `add` person.pos `sub`
                            (V3.scale 23 (Model.direction person))
                        cPos = V3.scale 0.1 newCameraPos `add` V3.scale 0.9 person.cameraPos
                        p = toRecord cPos
                        e = eyeLevel cPos
                        cameraPos = if p.y < e then vec3 p.x e p.z else cPos
                        newCameraUp = Model.cameraUp person

                    in  { person | cameraPos = Terrain.bounds placement cameraPos
                                 , cameraUp =
                            V3.scale 0.1 newCameraUp `add` V3.scale 0.9 person.cameraUp }
        in
            person0
                |> gravity eyeLevel inputs.dt
                |> selectVehicle inputs
                |> move
                |> bounds
                |> checkCamera
                |> moveCamera

selectVehicle : Model.Inputs -> Model.Person -> Model.Person
selectVehicle inputs person =
    let
        switch = inputs.button_X
        flying = switch `xor` person.flying
    in
        if not switch then
            person
        else if flying then
          Debug.log "Switch to flying!" <|
            DreamBird.welcome { person | flying = True }
        else
          Debug.log "Switch to buggy!" <|
            DreamBuggy.welcome { person | flying = False }

gravity : Model.EyeLevel -> Float -> Model.Person -> Model.Person
gravity eyeLevel dt person =
  if getY person.pos <= eyeLevel person.pos then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (v.y - 9.8 * dt) v.z }
