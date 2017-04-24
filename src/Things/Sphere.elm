module Things.Sphere exposing (spheres, skySphere, cloudsSphere, fogMountainsSphere, sphere)

import List exposing (drop, concat, map, map2)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

import Thing exposing (..)

import Shaders.Clouds exposing (clouds)
import Shaders.Sky exposing (sky)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

type alias Triangle a = (a,a,a)

spheres : Int -> Shader {} ThingShaderInput { elm_FragColor : Vec3, elm_FragCoord : Vec2 }
    -> List (Oriented (Visible {}))
spheres n fragmentShader = map (always (sphere worldVertex fragmentShader)) [0..n]

skySphere : Perception -> List Entity
skySphere = seeSphere worldVertex sky

cloudsSphere : Perception -> List Entity
cloudsSphere = seeSphere worldVertex clouds

fogMountainsSphere : Oriented (Visible {})
fogMountainsSphere = sphere worldVertex fogMountains

sphere : Shader Vertex ThingShaderInput a -> Shader {} ThingShaderInput a -> Oriented (Visible {})
sphere vertexShader fragmentShader =
    let see = seeSphere vertexShader fragmentShader
    in { scale = vec3 1 1 1, pos = vec3 0 0 0, orientation = vec3 1 0 1, see = see }

type alias ShadertoyUniforms a = { a | iResolution : Vec3, iGlobalTime : Float, view : (Int,Int) }

seeSphere : Shader Vertex ThingShaderInput a -> Shader {} ThingShaderInput a -> See
seeSphere vertexShader fragmentShader p =
    let resolution = vec3 (toFloat p.windowSize.width) (toFloat p.windowSize.height) 0
        s = p.globalTime
        iHMD = if p.cameraVR then 1.0 else 0.0
    in
        [entity vertexShader fragmentShader sphereMesh
            { iResolution=resolution, iGlobalTime=s, iHMD=iHMD
            , iLensDistort = p.lensDistort, view = p.viewMatrix }]

unfold : Int -> (a -> a) -> a -> List a
unfold n f x = if n==0 then [] else
  let res=f x in (res :: unfold (n-1) f res)

zip3 : List a -> List b -> List c -> List (a,b,c)
zip3 xs ys zs =
  case (xs, ys, zs) of
    (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
    _ -> []

rotY : Float -> Mat4
rotY n = makeRotate (2*pi/n) (vec3 0 1 0)

rotZ : Float -> Mat4
rotZ n = makeRotate (-2*pi/n) (vec3 0 0 1)

rotBoth : Float -> Vertex -> Vertex
rotBoth n x = { x | pos = transform (rotY n) x.pos, coord = transform (rotZ n) x.coord }

rotMercator : Float -> Vertex -> Vertex
rotMercator n v = { v | pos = transform (rotY n) v.pos,
    coord = vec3 (getX v.coord + (1.0/n)) (getY v.coord) 0 }

seven : Vertex -> List Vertex
seven = unfold 7 (rotMercator 8)

eights : Vertex -> (List Vertex, List Vertex)
eights x = let x7 = seven x in (x::x7, x7++[x])

unfoldMercator : Int -> Vertex -> List Vertex
unfoldMercator n = unfold (n-1) (rotMercator (toFloat n))

verticesMercator : Int -> Vertex -> (List Vertex, List Vertex)
verticesMercator n x = let xs = unfoldMercator n x in (x::xs, xs++[x])

sphereMesh : Mesh Vertex
sphereMesh =
  let
      white = vec3 1 1 1
      npole = { pos = vec3 0 1 0, coord = vec3 0 0 0 }
      spole = { pos = vec3 0 -1 0, coord = vec3 0 1 0 }

      nlat q = let x = sqrt (1-q*q) in { pos = vec3 x (-q) 0, color = white, coord = vec3 0 ((1-q)/2) 0 }
      slat q = let x = sqrt (1-q*q) in { pos = vec3 x q 0, color = white, coord = vec3 0 ((1+q)/2) 0 }

      nband q1 q2 = 
          let
              (band10, band11) = verticesMercator 20 (nlat q1)
              (band20, band21) = verticesMercator 20 (nlat q2)
              band1U = zip3 band10 band11 band20
              band1L = zip3 band20 band11 band21
          in band1U ++ band1L

      sband q1 q2 = 
          let
              (band10, band11) = verticesMercator 20 (slat q1)
              (band20, band21) = verticesMercator 20 (slat q2)
              band1U = zip3 band10 band11 band20
              band1L = zip3 band20 band11 band21
          in band1U ++ band1L

      qs0 n = map (\x -> x/n) [0..n]
      qs = map (sin << (\x -> x*pi/2)) (qs0 30)
      nbands = concat (map2 nband qs (drop 1 qs))
      sbands = concat (map2 sband qs (drop 1 qs))
  in
      Triangle <| nbands ++ sbands
