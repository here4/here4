module Shaders.FragmentPasta exposing (..)

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))


{-| Provides constant ambient
-}
fragment_ambient : GLSLPasta.Component
fragment_ambient =
    { empty
        | id = "lighting.fragment_ambient"
        , provides = [ "ambient" ]
        , requires = [ "diffuseColor" ]
        , globals =
            [ Uniform "vec4" "ambientColor"
            ]
        , splices =
            [ """
            // ambient
            vec3 ambient = ambientColor.rgb * diffuseColor;
"""
            ]
    }

