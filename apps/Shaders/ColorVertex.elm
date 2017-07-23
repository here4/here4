module Shaders.ColorVertex exposing
    ( perspective, distort, vertex_elm_FragColor, ColorVertex, colorVertex)


import GLSLPasta
import GLSLPasta.Core exposing (empty)
import GLSLPasta.Types as GLSLPasta exposing (Global(..), Dependencies(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)


type alias ColorVertex =
    { pos : Vec3, color : Vec3 }


{-| Generates position4
-}
vertex_pos4 : GLSLPasta.Component
vertex_pos4 =
    { empty
        | id = "vertex_pos4"
        , provides =
            [ "position4"
            ]
        , globals =
            [ Attribute "vec3" "pos"
            ]
        , splices =
            [ """
        vec4 pos4 = vec4(pos, 1.0);
            """
            ]
    }

perspective : GLSLPasta.Component
perspective =
    { empty
        | id = "perspective"
        , dependencies =
            Dependencies 
                [ vertex_pos4
                ]
        , provides =
            [ "gl_Position"
            ]
        , globals =
            [ Uniform "mat4" "iPerspective"
            , Uniform "mat4" "iLookAt"
            ]
        , splices =
            [ """
        vec4 p = iPerspective * iLookAt * pos4;
            """
            ]
    }

distort : GLSLPasta.Component
distort =
    { empty
        | id = "lighting.vertex_position4"
        , dependencies =
            Dependencies
                [ perspective
                ]
        , requires =
            [ "gl_Position"
            ]
        , globals =
            [ Uniform "float" "iLensDistort"
            ]
        , functions =
            [ """
vec4 distort(vec4 p)
{
  vec2 v = p.xy / p.w;

  // Convert to polar coords
  float theta = atan(v.y, v.x);
  float radius = length(v);

  // Distort
  radius = pow(radius, iLensDistort);

  // Convert back to Cartesian
  v.x = radius * cos(theta);
  v.y = radius * sin(theta);
  p.xy = v.xy * p.w;
  return p;
}
"""
            ]
        , splices =
            [ """
        if (iLensDistort > 0.0) {
          gl_Position = distort(p);
        } else {
          gl_Position = p;
        }
"""
            ]
    }

{-| Forward the vertex color to the fragment shader, as vcolor
-}
vertex_elm_FragColor : GLSLPasta.Component
vertex_elm_FragColor =
    { empty
        | id = "vcolor"
        , globals =
            [ Attribute "vec3" "color"
            , Varying "vec3" "elm_FragColor"
            ]
        , splices =
            [ """
        vcolor = color;
                """
            ]
    }


colorVertex : Shader ColorVertex { u | iLensDistort : Float, iPerspective : Mat4, iLookAt : Mat4 } { elm_FragColor : Vec3 }
colorVertex =
    GLSLPasta.combine [ perspective, distort, vertex_elm_FragColor ]
    |> WebGL.unsafeShader
