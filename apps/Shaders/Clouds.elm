module Shaders.Clouds exposing (clouds)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import WebGL exposing (..)

-- https://www.shadertoy.com/view/XslGRr
clouds : Shader {} { u | iResolution:Vec3, iGlobalTime:Float, iHMD:Float } { elm_FragColor:Vec3, elm_FragCoord:Vec2 }
clouds = [glsl|

precision mediump float;
uniform vec3 iResolution;
uniform float iGlobalTime;
uniform float iHMD;

varying vec3 elm_FragColor;
varying vec2 elm_FragCoord;

const vec2 Scale = vec2(0.1469278, 0.2350845);
//const vec2 Scale = vec2(0.1469278, 0.2350845);
const vec2 ScaleIn = vec2(3, 2.5);
//const vec2 ScaleIn = vec2(2.5, 1.5);
const vec4 HmdWarpParam   = vec4(1, 0.22, 0.24, 0);

// Scales input texture coordinates for distortion.
vec2 HmdWarp(vec2 in01, vec2 LensCenter)
{
	vec2 theta = (in01 - LensCenter) * ScaleIn; // Scales to [-1, 1]
	float rSq = theta.x * theta.x + theta.y * theta.y;
	vec2 rvector = theta * (HmdWarpParam.x + HmdWarpParam.y * rSq +
		HmdWarpParam.z * rSq * rSq +
		HmdWarpParam.w * rSq * rSq * rSq);
	return LensCenter + Scale * rvector;
}

// Created by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// hash based 3d value noise
float hash( float n )
{
    return fract(sin(n)*43758.5453);
}
float noise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);

    f = f*f*(3.0-2.0*f);
    float n = p.x + p.y*57.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+ 57.0), hash(n+ 58.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+170.0), hash(n+171.0),f.x),f.y),f.z);
}

vec4 map( in vec3 p )
{
    float d = 0.2 - p.y;

    vec3 q = p - vec3(1.0,0.1,0.0)*iGlobalTime;
    float f;
    f  = 0.5000*noise( q ); q = q*2.02;
    f += 0.2500*noise( q ); q = q*2.03;
    f += 0.1250*noise( q ); q = q*2.01;
    f += 0.0625*noise( q );

    d += 3.0 * f;

    d = clamp( d, 0.0, 1.0 );
    
    vec4 res = vec4( d );

    res.xyz = mix( 1.15*vec3(1.0,0.95,0.8), vec3(0.7,0.7,0.7), res.x );
    
    return res;
}


vec3 sundir = vec3(-1.0,0.0,0.0);


vec4 raymarch( in vec3 ro, in vec3 rd )
{
    vec4 sum = vec4(0, 0, 0, 0);

    float t = 0.0;
    for(int i=0; i<64; i++)
    {
        if( sum.a > 0.99 ) continue;

        vec3 pos = ro + t*rd;
        vec4 col = map( pos );
        
        float dif =  clamp((col.w - map(pos+0.3*sundir).w)/0.6, 0.0, 1.0 );

                vec3 lin = vec3(0.65,0.68,0.7)*1.35 + 0.45*vec3(0.7, 0.5, 0.3)*dif;
        col.xyz *= lin;
        
        col.a *= 0.35;
        col.rgb *= col.a;

        sum = sum + col*(1.0 - sum.a);    

        t += max(0.1, 0.025*t);
    }

    sum.xyz /= (0.001+sum.w);

    return clamp( sum, 0.0, 1.0 );
}

void clouds(vec2 tc)
{
    // vec2 q = gl_FragCoord.xy / iResolution.xy;
    vec2 q = tc.xy;
    vec2 p = -1.0 + 2.0*q;
    //p.x *= iResolution.x/ iResolution.y;
    p.x *= 1.5;
    //vec2 mo = -1.0 + 2.0*iMouse.xy / iResolution.xy;
    
    // camera
    //vec3 ro = 4.0*normalize(vec3(cos(2.75-3.0*mo.x), 0.7+(mo.y+1.0), sin(2.75-3.0*mo.x)));
    vec3 ro = 4.0*normalize(vec3(cos(2.75-1.3), 0.7, sin(2.75-2.0)));
    vec3 ta = vec3(0.0, 1.0, 0.0);
    vec3 ww = normalize( ta - ro);
    vec3 uu = normalize(cross( vec3(0.0,1.0,0.0), ww ));
    vec3 vv = normalize(cross(ww,uu));
    vec3 rd = normalize( p.x*uu + p.y*vv + 1.5*ww );
    
    vec4 res = raymarch( ro, rd );

    float sun = clamp( dot(sundir,rd), 0.0, 1.0 );
    vec3 col = vec3(0.6,0.71,0.75) - rd.y*0.2*vec3(1.0,0.5,1.0) + 0.15*0.5;
    col += 0.2*vec3(1.0,.6,0.1)*pow( sun, 8.0 );
    col *= 0.95;
    col = mix( col, res.xyz, res.w );
    col += 0.1*vec3(1.0,0.4,0.2)*pow( sun, 3.0 );

    gl_FragColor = vec4( col, 1.0 );
}

void hmd(void)
{
    vec2 LensCenter = vec2(0.5, 0.5);
    vec2 ScreenCenter = vec2(0.5, 0.5);

    vec2 oTexCoord = gl_FragCoord.xy / iResolution.xy;

    vec2 tc = HmdWarp(oTexCoord, LensCenter);
    if (any(bvec2(clamp(tc,ScreenCenter-vec2(0.5,0.5), ScreenCenter+vec2(0.5,0.5)) - tc)))
    {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
        return;
    }

    clouds(tc);
}

void main() {
    if (iHMD == 1.0)
        hmd();
    else
        clouds(elm_FragCoord);
}
|]