module Shaders.FragmentPasta exposing (..)

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))

{-| Provides constant ambient
-}
fragment_ambient_07 : GLSLPasta.Component
fragment_ambient_07 =
    { empty
        | id = "lighting.fragment_ambient_03"
        , provides = [ "ambient" ]
        , requires = [ "diffuseColor" ]
        , globals = []
        , splices =
            [ """
            // ambient
            vec3 ambient = 0.7 * diffuseColor;
"""
            ]
    }


