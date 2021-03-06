module Shaders.Kintsugi exposing (kintsugi)

import GLSLPasta exposing (empty)
import GLSLPasta.Lighting as Lighting
import GLSLPasta.Types as GLSLPasta exposing (Global(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Shaders.FragmentPasta exposing (..)
import Shaders.HMD exposing (hmdTemplate)
import WebGL exposing (..)


-- https://www.shadertoy.com/view/Xt33WX


fragment_kintsugi : GLSLPasta.Component
fragment_kintsugi =
    { empty
        | id = "fragment_kintsugi"
        , provides = [ "diffuseColor" ]
        , globals =
            [ Uniform "vec3" "iResolution"
            , Uniform "float" "iGlobalTime"
            , Varying "vec4" "elm_FragColor"
            , Varying "vec2" "elm_FragCoord"
            ]
        , functions =
            [ """
//
// Description : Array and textureless GLSL 2D simplex noise function.
//      Author : Ian McEwan, Ashima Arts.
//  Maintainer : stegu
//     Lastmod : 20110822 (ijm)
//     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
//               Distributed under the MIT License. See LICENSE file.
//               https://github.com/ashima/webgl-noise
//               https://github.com/stegu/webgl-noise
//

vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec2 mod289(vec2 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 permute(vec3 x) {
  return mod289(((x*34.0)+1.0)*x);
}

float snoise(vec2 v)
  {
  const vec4 C = vec4(0.211324865405187,  // (3.0-sqrt(3.0))/6.0
                      0.366025403784439,  // 0.5*(sqrt(3.0)-1.0)
                     -0.577350269189626,  // -1.0 + 2.0 * C.x
                      0.024390243902439); // 1.0 / 41.0
// First corner
  vec2 i  = floor(v + dot(v, C.yy) );
  vec2 x0 = v -   i + dot(i, C.xx);

// Other corners
  vec2 i1;
  //i1.x = step( x0.y, x0.x ); // x0.x > x0.y ? 1.0 : 0.0
  //i1.y = 1.0 - i1.x;
  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  // x0 = x0 - 0.0 + 0.0 * C.xx ;
  // x1 = x0 - i1 + 1.0 * C.xx ;
  // x2 = x0 - 1.0 + 2.0 * C.xx ;
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;

// Permutations
  i = mod289(i); // Avoid truncation effects in permutation
  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
        + i.x + vec3(0.0, i1.x, 1.0 ));

  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
  m = m*m ;
  m = m*m ;

// Gradients: 41 points uniformly over a line, mapped onto a diamond.
// The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287)

  vec3 x = 2.0 * fract(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;

// Normalise gradients implicitly by scaling m
// Approximation of: m *= inversesqrt( a0*a0 + h*h );
  m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );

// Compute final noise value at P
  vec3 g;
  g.x  = a0.x  * x0.x  + h.x  * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}

float turbulence (vec2 P)
{
    float val = 0.0;
    float freq = 0.7;
    for (int i=0; i<5; i++) {
        val += snoise (P*freq);
        //freq *= 2.07;
        freq *= 1.57;
    }
    return val;
}

// Expects -1<x<1
vec4 marble_color (float x)
{
    vec4 col;
    x = 0.5 * (x + 1.0);      // transform -1<x<1 to 0<x<1
    for (int i=0; i<3; i++) { // make x fall of rapidly...
        x = sqrt(x);
    }

    if(x <= 0.45) {
        col.r = 1.0;
        col.g = 0.84;
        col.b = 0.0;
        col *= vec4(1.95 - x) * 0.55;
        col.a = 1.0;
    }
    else {
        col = vec4(x);
    }

    col.r = min(col.r, 1.0);
    col.g = min(col.g, 1.0);
    col.b = min(col.b, 1.0);

    return col;
}

vec4 kintsugi(vec2 uv)
{
    float amplitude = 2.0;

    vec2 m;
    m.x = 10.0;
    m.y = 20.0;

    float t = uv.x * 10.0;
    t += amplitude * turbulence (uv.xy + vec2(iGlobalTime / 80.0) - m );
    t = sin(t);
    return marble_color(t);
}
"""
            ]
        , splices =
            [ """
            vec3 diffuseColor = kintsugi(fragCoord).rgb;
"""
            ]
    }


kintsugi : Shader {} { u | iResolution : Vec3, iGlobalTime : Float } { elm_FragColor : Vec4, elm_FragCoord : Vec2, clipPosition : Vec4 }
kintsugi =
    GLSLPasta.combineUsingTemplate hmdTemplate
        "kintsugi"
        [ Lighting.fragment_lightDir
        , Lighting.fragment_interpolatedNormal
        , Lighting.fragment_lambert
        , fragment_kintsugi
        , Lighting.fragment_diffuse
        , fragment_ambient
        , Lighting.fragment_specular
        , Lighting.fragment_attenuation
        , Lighting.fragment_phong
        , Lighting.lightenDistance
        ]
        |> WebGL.unsafeShader
