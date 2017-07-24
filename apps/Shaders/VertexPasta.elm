module Shaders.VertexPasta
    exposing
        ( perspective
        , distort
        , vertex_elm_FragColor
        , vertex_elm_FragCoord
        , vertex_pos4
        , vertex_noise
        , vertex_ripple
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

{-| Forward the vertex color to the fragment shader, as vcolor
-}
vertex_elm_FragColor : GLSLPasta.Component
vertex_elm_FragColor =
    { empty
        | id = "elm_FragColor"
        , globals =
            [ Attribute "vec4" "color"
            , Varying "vec4" "elm_FragColor"
            ]
        , splices =
            [ """
        elm_FragColor = color;
                """
            ]
    }

vertex_elm_FragCoord : GLSLPasta.Component
vertex_elm_FragCoord =
    { empty
        | id = "elm_FragCoord"
        , globals =
            [ Attribute "vec3" "coord"
            , Varying "vec2" "elm_FragCoord"
            ]
        , splices =
            [ """
        elm_FragCoord = coord.xy;
"""
            ]
    }


vertex_noise : GLSLPasta.Component
vertex_noise =
    { empty
        | id = "vertex_noise"
        , globals =
            [ Attribute "float" "textureScale"
            , Attribute "float" "timeScale"
            , Attribute "float" "smoothing"
            , Varying "float" "iTextureScale"
            , Varying "float" "iTimeScale"
            , Varying "float" "iSmoothing"
            ]
        , splices =
            [ """
        iTextureScale = textureScale;
        iTimeScale = timeScale;
        iSmoothing = smoothing;
"""
            ]
    }

vertex_ripple : GLSLPasta.Component
vertex_ripple =
    { empty
        | id = "vertex_ripple"
        , dependencies =
            Dependencies
                [ vertex_pos4
                ]
        , globals =
            [ Attribute "vec3" "pos"
            , Attribute "vec3" "coord"
            , Uniform "float" "iGlobalTimeV"
            , Uniform "float" "iRipple"
            ]
        , splices =
            [ """
        pos4.y += iRipple * sin(coord.x*coord.y + iGlobalTimeV);
"""
            ]
    }

