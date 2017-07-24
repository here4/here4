module Shaders.VertexPasta
    exposing
        ( perspective
        , distort
        , vertex_elm_FragColor
        )

import GLSLPasta
import GLSLPasta.Core exposing (empty)
import GLSLPasta.Types as GLSLPasta exposing (Global(..), Dependencies(..))


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
