module Shaders.Obj exposing (..)

import GLSLPasta
import GLSLPasta.Lighting as Lighting
import WebGL


{-| This shader uses Spherical Environment Mapping (SEM).
Here are some relevant links:

  - [very cool demo](https://www.clicktorelease.com/code/spherical-normal-mapping/#)
  - <https://www.clicktorelease.com/blog/creating-spherical-environment-mapping-shader>
  - <http://www.ozone3d.net/tutorials/glsl_texturing_p04.php>

-}
reflectionVert =
    GLSLPasta.combine "reflectionVert"
        [ Lighting.vertexReflection
        , Lighting.vertex_clipPosition
        ]
    |> WebGL.unsafeShader


reflectionFrag =
    GLSLPasta.combine "reflectionFrag"
        [ Lighting.fragmentReflection
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader


{-| normal mapping according to:
<http://www.gamasutra.com/blogs/RobertBasler/20131122/205462/Three_Normal_Mapping_Techniques_Explained_For_the_Mathematically_Uninclined.php?print=1>
-}
normalVert =
    GLSLPasta.combine "normalVert"
        [ Lighting.vertexNormal
        , Lighting.vertex_clipPosition
        ]
        |> WebGL.unsafeShader


normalFrag =
    GLSLPasta.combine "normalFrag"
        [ Lighting.fragmentNormal
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader


{-| same as the normal mapping shader, but without deforming normals.
-}
noNormalVert =
    GLSLPasta.combine "noNormalVert"
        [ Lighting.vertexNoNormal
        , Lighting.vertex_clipPosition
        ]
        |> WebGL.unsafeShader


noNormalFrag =
    GLSLPasta.combine "noNormalFrag"
        [ Lighting.fragmentNoNormal
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader


{-| same as above, but without any textures.
-}
simpleVert =
    GLSLPasta.combine "simpleVert"
        [ Lighting.vertexSimple
        , Lighting.vertex_clipPosition
        ]
        |> WebGL.unsafeShader


simpleFrag =
    GLSLPasta.combine "simpleFrag"
        [ Lighting.fragmentSimple
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader
