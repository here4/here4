module Update exposing (update)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (makeRotate, transform)

import Model
import Ports

{-| Take a Msg and a Model and return an updated Model
-}
update : Model.Msg -> Model.Model -> (Model.Model, Cmd Model.Msg)
update msg model =
    case msg of
        Model.TextureError err ->
            ( { model | message = "Error loading texture" }, Cmd.none )
        Model.TextureLoaded texture ->
            ( { model | maybeTexture = Just texture }, Cmd.none )
        Model.TerrainGenerated terrain ->
            ( { model | maybeTerrain = Just terrain }, Cmd.none )
        Model.KeyChange keyfunc ->
            ( { model | keys = keyfunc model.keys }, Cmd.none )
        Model.Resize windowSize ->
            ( { model | maybeWindowSize = Just windowSize }, Cmd.none )
        Model.MouseMove movement ->
            ( { model | person = turn movement model.person }, Cmd.none )
        Model.LockRequest wantToBeLocked ->
            ( { model | wantToBeLocked = wantToBeLocked }
            , if model.wantToBeLocked == model.isLocked then
                Cmd.none
              else if model.wantToBeLocked then
                Ports.requestPointerLock ()
              else
                Ports.exitPointerLock ()
            ) 
        Model.LockUpdate isLocked ->
            ( { model | isLocked = isLocked }, Cmd.none )
        Model.Animate dt ->
            ( { model | lifetime = model.lifetime + dt
                      , person = model.person
                          |> walk (directions model.keys)
                          |> jump model.keys.space
                          |> gravity (dt / 500)
                          |> physics (dt / 500)
              }
            , Cmd.none )

directions : Model.Keys -> { x : Int, y : Int }
directions { left, right, up, down } =
    let dir a b = case (a,b) of
            (True, False) -> -1
            (False, True) -> 1
            _             -> 0
    in { x = dir left right, y = dir down up }

flatten : Vec3 -> Vec3
flatten v =
    let r = toRecord v
    in  normalize (vec3 r.x 0 r.z)

turn : Model.MouseMovement -> Model.Person -> Model.Person
turn (dx,dy) person =
    let yo x = toFloat (clamp -10 10 x) / 500
        h' = person.horizontalAngle + yo dx
        v' = person.verticalAngle   - yo dy
    in
        { person | horizontalAngle = h'
                 , verticalAngle = clamp (degrees -45) (degrees 45) v'
        }

walk : { x:Int, y:Int } -> Model.Person -> Model.Person
walk directions person =
  if getY person.position > Model.eyeLevel then person else
    let moveDir = normalize (flatten (Model.direction person))
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (toFloat directions.y) moveDir
        strafe = V3.scale (toFloat directions.x) strafeDir
    in
        { person | velocity = adjustVelocity (move `add` strafe) }

adjustVelocity : Vec3 -> Vec3
adjustVelocity v =
    case toTuple v of
      (0,0,0) -> v
      _       -> V3.scale 2 (normalize v)

jump : Bool -> Model.Person -> Model.Person
jump isJumping person =
  if not isJumping || getY person.position > Model.eyeLevel then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x 2 v.z }

physics : Float -> Model.Person -> Model.Person
physics dt person =
    let position = person.position `add` V3.scale dt person.velocity
        p = toRecord position

        position' = if p.y < Model.eyeLevel
                    then vec3 p.x Model.eyeLevel p.z
                    else position
    in
        { person | position = position' }

gravity : Float -> Model.Person -> Model.Person
gravity dt person =
  if getY person.position <= Model.eyeLevel then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (v.y - 2 * dt) v.z }

{-
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Orientation
import Util exposing (v3_clamp)

import Array2D exposing (Array2D)
import Model
import Things.Surface2D exposing (Placement)
import Things.Terrain as Terrain
import Vehicles.DreamBird as DreamBird
import Vehicles.DreamBuggy as DreamBuggy
import Vehicles.DreamDebug as DreamDebug

import Debug

aboveTerrain : Model.EyeLevel -> Vec3 -> Vec3
aboveTerrain eyeLevel pos =
    let
        p = toRecord pos
        e = eyeLevel pos
    in
        if p.y < e then vec3 p.x e p.z else pos

step : Placement -> Array2D Float -> Model.Inputs -> Model.Person -> Model.Person
step placement terrain inputs person0 = if inputs.reset then Model.defaultPerson else
        let 
            eyeLevel pos = Model.eyeLevel + Terrain.elevation placement terrain pos
            move person =
                if person.vehicle == Model.vehicleBird then
                      DreamBird.move eyeLevel inputs person
                else if person.vehicle == Model.vehicleBuggy then
                      DreamBuggy.move eyeLevel inputs person
                else
                      DreamDebug.move eyeLevel inputs person
            bounds person = { person | pos = Terrain.bounds placement person.pos }

            checkCamera person = { person |
                cameraInside = if inputs.changeCamera then
                                   not person.cameraInside
                               else
                                   person.cameraInside,
                cameraVR = if inputs.changeVR then
                                   not person.cameraVR
                               else
                                   person.cameraVR }

            moveCamera person =
                if person.cameraInside then
                    -- let behind = person.pos `sub` (V3.scale 2.5 (Model.direction person)) `sub` (vec3 0 0.5 0)
                    let inside = person.pos
                                     `add` Orientation.rotateBodyV person.orientation (vec3 0 0 1) -- wedge
                                     -- Inside Jeep driver's seat
                                     -- `add` Qn.vrotate person.orientQn (vec3 0.38 0.5 -2.3)
                    in
                        { person | cameraPos = inside -- aboveTerrain eyeLevel behind
                                 , cameraUp = Model.cameraUp person }
                else
                    let behind = person.pos `sub` (V3.scale 7 (Model.direction person))
                        p = toRecord person.pos
                        yMax0 v = let vr = V3.toRecord v in vec3 vr.x (min (-0.3) vr.y) vr.z
                        newCameraPos =
                            if p.y < Model.eyeLevel then
                                yMax0 (vec3 0 2 0 `add` behind)
                            else if p.y < Model.eyeLevel+1 then
                                behind
                            else
                                -- vec3 0 2 0 `add` behind
                                vec3 0 -2 0 `add` behind
                        cameraPos = aboveTerrain eyeLevel
                            -- (V3.scale 0.5 newCameraPos `add` V3.scale 0.5 person.cameraPos) -- smooth
                            newCameraPos
                        newCameraUp = Model.cameraUp person

                    in  { person | cameraPos = Terrain.bounds placement cameraPos
                                 , cameraUp =
                            -- V3.scale 0.1 newCameraUp `add` V3.scale 0.9 person.cameraUp }
                               newCameraUp }
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
        newVehicle = Model.nextVehicle person.vehicle
    in
        if not switch then
            person
        else if newVehicle == Model.vehicleBuggy then
            Debug.log "Switch to buggy!" <|
                DreamBuggy.welcome { person | vehicle = newVehicle }
        else if newVehicle == Model.vehicleBird then
            Debug.log "Switch to flying!" <|
                DreamBird.welcome { person | vehicle = newVehicle }
        -- else if newVehicle == vehicleDebug then
        else
            Debug.log "Switch to debug!" <|
                DreamDebug.welcome { person | vehicle = newVehicle }

gravity : Model.EyeLevel -> Float -> Model.Person -> Model.Person
gravity eyeLevel dt person =
  if getY person.pos <= eyeLevel person.pos then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (v.y - 9.8 * dt) v.z }
-}
