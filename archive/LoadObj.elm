-- module LoadObj (loadObj) where
module LoadObj exposing (..)

import String
import Array
import Maybe.Extra exposing (maybeToList)
import Task exposing (Task, andThen)


import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Matrix4 as M4
import WebGL exposing (..)

import Engine exposing (..)

import Debug

type alias VertV = {position: Vec3}
type alias VertVT = {position : Vec3, texCoord : Vec3}
type alias VertVN = {position : Vec3, normal : Vec3}
type alias VertVTN = {position : Vec3, texCoord : Vec3, normal : Vec3}

type alias Triple a = (a,a,a)

type FaceVert =
      FaceVertV VertV
    | FaceVertVT VertVT
    | FaceVertVN VertVN
    | FaceVertVTN VertVTN

toVTN f = case f of
    FaceVertVTN vtn -> vtn
    FaceVertVN vn   -> { position = vn.position, normal = vn.normal, texCoord = vec3 0 0 0 }
    FaceVertV v     -> { position = v.position, normal = vec3 0 1 0, texCoord = vec3 0 0 0 }
    FaceVertVT vt   -> { position = vt.position, normal = vec3 0 1 0, texCoord = vt.texCoord }

type Face =
      FaceV (VertV, VertV, VertV) 
    | FaceVT (VertVT, VertVT, VertVT)
    | FaceVN (VertVN, VertVN, VertVN)
    | FaceVTN (VertVTN, VertVTN, VertVTN)

--Used to avoid "Math.pow is not a function"
myPow : Float -> Int -> Float
myPow b e =
    if e < 0 then
        1.0 / (myPow b (0-e))
    else if e == 0 then
        1
    else
        b * (myPow b (e-1))

--Check if a float is in scientific notation, then parse it accordingly
parseFloat : String -> Maybe Float
parseFloat s = Result.toMaybe <| case String.split "e" s of
  [fs] -> String.toFloat fs
  [base, power] -> let
      bfR = String.toFloat base
      piR = String.toInt power
    in Result.map2 (\bf pi -> bf * (myPow 10.0 pi)) bfR piR
  _ -> Err ""


--Check the first character of a line for parsing
isVertexLine s = Maybe.withDefault False <| Maybe.map (\w -> w == "v") (List.head <| String.words s)

isVtLine= (\s -> String.startsWith "vt" s)
isVnLine= (\s -> String.startsWith "vn" s)
isFaceLine = (\s -> String.startsWith "f" s)

lineToVert : String -> Maybe Vec3
lineToVert line = case (String.words line) of
    ["v", v1, v2, v3] -> Maybe.map3 vec3 (parseFloat v1)  (parseFloat v2) (parseFloat v3) 
    _                 -> Nothing
  
lineToVn line = case (String.words line) of
    ["vn", v1, v2, v3] -> Maybe.map3 vec3 (parseFloat v1)  (parseFloat v2) (parseFloat v3)
    _                  -> Nothing
  
lineToVt line = case (String.words line) of
    ["vt", v1, v2, v3] -> Maybe.map3 vec3 (parseFloat v1)  (parseFloat v2) (parseFloat v3)
    ["vt", v1, v2]     -> Maybe.map2 (\x y -> vec3 x y 0.0) (parseFloat v1)  (parseFloat v2)
    _                  -> Nothing

-- lineToFace : (Array.Array Vec3, Array.Array Vec3, Array.Array Vec3) -> String -> List (Triple FaceVert)
lineToFace : Array.Array Vec3 -> Array.Array Vec3 -> Array.Array Vec3 -> String -> List (Triple FaceVert)
-- lineToFace arrs line =
lineToFace vArr vtArr vnArr line =
    let
        -- parse = parseFaceVert arrs
        parse = parseFaceVert vArr vtArr vnArr
    in
        Maybe.withDefault [] <| case (String.words line) of
            ["f", f1, f2, f3] -> Maybe.map3 (\p1 p2 p3 -> [(p1, p2, p3)]) (parse f1) (parse f2) (parse f3)
            ["f", f1, f2, f3, f4] -> Maybe.map4 (\p1 p2 p3 p4 -> [(p1, p2, p3), (p2, p3, p4)]) (parse f1) (parse f2) (parse f3) (parse f4)
            _ -> Nothing

--Eventually will look for normals and such
--Right now, just converts the string to the vertex index
-- parseFaceVert : (Array.Array Vec3, Array.Array Vec3, Array.Array Vec3) -> String -> Maybe FaceVert
parseFaceVert : Array.Array Vec3 -> Array.Array Vec3 -> Array.Array Vec3 -> String -> Maybe FaceVert
parseFaceVert vArr vtArr vnArr str = case String.split "//" str of
  [v, n] -> Maybe.map FaceVertVN (Maybe.map2 VertVN (deIndexVert vArr v) (deIndexVert vnArr n)) --vertex and normal
  [s]    -> case String.split "/" s of
    [v]     -> Maybe.map (FaceVertV << VertV) (deIndexVert vArr v) --only vertex
    [v,t]   -> Maybe.map FaceVertVT <| Maybe.map2 VertVT (deIndexVert vArr v) (deIndexVert vtArr t) --vertex and tex coords
    [v,t,n] -> Maybe.map FaceVertVTN <| Maybe.map3 VertVTN (deIndexVert vArr v) (deIndexVert vtArr t) (deIndexVert vnArr n) --all 3.
    _       -> Nothing
  _      -> Nothing

--Given 3 indices and vertex array, get the right vertex from the array
-- deIndexVert vArr s = Result.map (\i -> Array.get (i-1) vArr) <| String.toInt s

deIndexVert : Array.Array Vec3 -> String -> Maybe Vec3
deIndexVert vArr s = Maybe.andThen (Result.toMaybe (String.toInt s)) (\i -> Array.get (i-1) vArr)

normalize : List Vec3 -> List Vec3
normalize list =
    let
        findMinMax minCoord maxCoord list = case list of
            [] -> (minCoord, maxCoord)
            (v :: vs) ->
                let r = V3.toRecord v
                    minCoord' = min minCoord (min r.x (min r.y r.z))
                    maxCoord' = max maxCoord (max r.x (max r.y r.z))
                in findMinMax minCoord' maxCoord' vs
        (minCoord, maxCoord) = Debug.log "findMinMax" <| findMinMax 1000000 -1000000 list
        range = maxCoord - minCoord
        norm x = 4 * (x - minCoord) / range
        normV v = let r = V3.toRecord v in vec3 (norm r.x) (norm r.y) (norm r.z)
    in
        List.map normV list

recenter : List Vec3 -> List Vec3
recenter list =
    let
        fltMax = 1000000
        fltMin = -1000000
        findMinMax minX maxX minY maxY minZ maxZ list = case list of
            [] -> (minX, maxX, minY, maxY, minZ, maxZ)
            (v :: vs) ->
                let r = V3.toRecord v
                    minX' = min minX r.x
                    maxX' = max maxX r.x
                    minY' = min minY r.y
                    maxY' = max maxY r.y
                    minZ' = min minZ r.z
                    maxZ' = max maxZ r.z
                in findMinMax minX' maxX' minY' maxY' minZ' maxZ' vs
        (minX, maxX, minY, maxY, minZ, maxZ) = Debug.log "findMinMax" <| findMinMax fltMax fltMin fltMax fltMin fltMax fltMin list
        rangeX = maxX - minX
        rangeY = maxY - minY
        rangeZ = maxZ - minZ
        cX x = x - minX - (rangeX/2)
        cY y = y - minY - (rangeY/2)
        cZ z = z - minZ - (rangeZ/2)
        centerV v = let r = V3.toRecord v in vec3 (cX r.x) (cY r.y) (cZ r.z)
    in
        List.map centerV list

--Parse an OBJ file into a list of triangles  
parseObj : String -> List (Triple FaceVert)
parseObj inFile = 
  let 
    lines = String.lines inFile
    
    vLines = List.filter isVertexLine lines
    vertices = Array.fromList <| normalize <| recenter <| List.filterMap lineToVert vLines
    
    vtLines = List.filter isVtLine lines
    texCoords = Array.fromList <| List.filterMap lineToVt vtLines
    
    vnLines = List.filter isVnLine lines
    normals = Array.fromList <| List.filterMap lineToVn vnLines
    
    fLines = List.filter isFaceLine lines
    faces = List.concat <| List.map (lineToFace vertices texCoords normals) fLines
    
  in faces

mesh inFile = Triangle <| List.map (\(a,b,c) -> (toVTN a, toVTN b, toVTN c)) <| parseObj inFile

{-
fromResponse r = case r of 
  Http.Success s -> s
  _ -> ""

inFileSig infile = let
    resp = Http.get <| Signal.constant "resources/wt_teapot.obj"
  in Signal.map fromResponse resp
-}

objMailbox : Signal.Mailbox String
objMailbox = Signal.mailbox ""

sendRaw : Signal.Mailbox String -> String -> Task x ()
sendRaw mb plush = Signal.send mb.address plush
 
-- loadMesh infile = Signal.map mesh (inFileSig infile)
loadMesh infile = Signal.map mesh objMailbox.signal


objThing : Mesh VertVTN -> Perception -> List Entity
objThing mesh p = [entity vertexShader fragmentShader mesh { view = p.viewMatrix }]

loadObj infile = Signal.map objThing (loadMesh infile)

objJeepMailbox : Signal.Mailbox String
objJeepMailbox = Signal.mailbox ""

loadJeepMesh = Signal.map mesh objJeepMailbox.signal

jeepThing : Mesh VertVTN -> Perception -> List Entity
jeepThing mesh p =
    let
        rv = p.viewMatrix
             |> M4.translate (vec3 -2 -2 0)
             |> M4.rotate (degrees -90) V3.i
    in
        [entity vertexShader fragmentShader mesh { view = rv }]

loadJeep infile = Signal.map jeepThing loadJeepMesh

--Based off the triangle rendering code from http://elm-lang.org/edit/examples/WebGL/Triangle.elm
  
-- Create the scene

--main : Signal Element

{-
main = Signal.map scene meshSig (foldp (+) 0 (fps 30))

scene : [Triangle VertVTN] -> Float -> Element
scene teapot t =
    webgl (400,400)
    [ entity vertexShader fragmentShader teapot { view = view (t / 1000), m_normal = normal (t / 1000) } ]

view : Float -> Mat4
view t =
    mul (makePerspective 45 1 0.01 100)
        (makeLookAt (vec3 (3 * cos t) 0 ((3 * sin t))) (vec3 0 0 0) (vec3 0 1 0))

normal t = transpose <| inverseOrthonormal <| view t

-}


-- Shaders

vertexShader : Shader VertVTN { unif | view:Mat4 } { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 texCoord;
attribute vec3 normal;
uniform mat4 view;
//uniform mat4 m_normal;
varying vec3 vcolor;

void main () {
    gl_Position = view * vec4(position, 1.0);
    
    //vec4 n1 = normalize(m_normal * vec4(normal, 1.0) );
    //vec3 n = n1.xyz;
    vec3 n = normal;
    
    //http://www.lighthouse3d.com/tutorials/glsl-core-tutorial/directional-lights/
    
    vec3 diffuse = vec3(1,1,1);
    
    vec3 l_dir = vec3(3,0,0);
    
    // compute the intensity as the dot product
    // the max prevents negative intensity values
    float intensity = max(dot(n, l_dir), 0.0);
 
    // Compute the color per vertex
    vcolor = intensity * diffuse;
    
    
}

|]

fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]  
