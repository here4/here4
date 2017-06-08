module Camera.POV exposing (pov)

import Body exposing (Oriented)
import Camera exposing (Camera)
import Camera.Util exposing (toCamera)

pov : Oriented a -> Camera
pov thing = toCamera thing
