module Shaders.VertexPasta
    exposing
        ( distort
        , vertex_elm_FragColor
        , vertex_elm_FragCoord
        , vertex_noise
        , vertex_ripple
        )

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..), Dependencies(..))


distort : GLSLPasta.Component
distort =
    { empty
        | id = "lighting.vertex_position4"
        , dependencies =
            Dependencies
                [ Lighting.vertex_gl_Position
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
          gl_Position = distort(gl_Position);
        }
"""
            ]
    }


{-| Forward the vertex color to the fragment shader, as vec4 elm_FragColor
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
                [ Lighting.vertex_position4
                ]
        , globals =
            [ Attribute "vec3" "coord"
            , Uniform "float" "iGlobalTimeV"
            , Uniform "float" "iRipple"
            ]
        , splices =
            [ """
        position4.y += iRipple * sin(coord.x*coord.y + iGlobalTimeV);
"""
            ]
    }
