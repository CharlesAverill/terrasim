#version 460 core
// https://www.shadertoy.com/view/Ml2XRV#

in vec2 fragUV;
out vec4 fragColor;

uniform int iTime;
uniform vec2 iResolution;   // Screen resolution
uniform sampler2D iChannel0; // Input texture (tilemap/biome/etc.)
uniform float iLat;  // latitude rotation in radians
uniform float iLon;  // longitude rotation in radians

// Rotation around the X axis
mat3 xrot(float angle) {
    float c = cos(angle);
    float s = sin(angle);
    return mat3(
        1.0, 0.0, 0.0,
        0.0,    c,   -s,
        0.0,    s,    c
    );
}

// Rotation around the Y axis
mat3 yrot(float angle) {
    float c = cos(angle);
    float s = sin(angle);
    return mat3(
          c, 0.0,  -s,
        0.0, 1.0, 0.0,
          s, 0.0,   c
    );
}

// Sphere-ray intersection. Returns distance if hit, -1.0 otherwise.
float intersectSphere(vec3 camera, vec3 ray, vec3 sphereOrigin, float sphereRadius) {
    float radiusSquared = sphereRadius * sphereRadius;
    float dt = dot(ray, sphereOrigin - camera);
    if (dt < 0.0) return -1.0;

    vec3 tmp = camera - sphereOrigin;
    float d2 = dot(tmp, tmp) - dt * dt;
    if (d2 >= radiusSquared) return -1.0;

    return dt - sqrt(radiusSquared - d2);
}

/// Stars
// https://www.shadertoy.com/view/3dSXWt
// divisions of grid
const float repeats = 30.;

// number of layers
const float layers = 21.;

// star colours
const vec3 blue = vec3(51.,64.,195.)/255.;
const vec3 cyan = vec3(117.,250.,254.)/255.;
const vec3 white = vec3(255.,255.,255.)/255.;
const vec3 yellow = vec3(251.,245.,44.)/255.;
const vec3 red = vec3(247,2.,20.)/255.;

// spectrum function
vec3 spectrum(vec2 pos){
    pos.x *= 4.;
    vec3 outCol = vec3(0);
    if( pos.x > 0.){
        outCol = mix(blue, cyan, fract(pos.x));
    }
    if( pos.x > 1.){
        outCol = mix(cyan, white, fract(pos.x));
    }
    if( pos.x > 2.){
        outCol = mix(white, yellow, fract(pos.x));
    }
    if( pos.x > 3.){
        outCol = mix(yellow, red, fract(pos.x));
    }
    
    return 1.-(pos.y * (1.-outCol));
}

float N21(vec2 p){
    p = fract(p*vec2(233.34, 851.73));
    p+= dot(p, p+23.45);
    return fract(p.x*p.y);
}

vec2 N22 (vec2 p){
	float n = N21(p);
    return vec2 (n, N21(p+n));
}

mat2 scale(vec2 _scale){
    return mat2(_scale.x,0.0,
                0.0,_scale.y);
}

// 2D Noise based on Morgan McGuire @morgan3d
// https://www.shadertoy.com/view/4dS3Wd
float noise (in vec2 st) {
    vec2 i = floor(st);
    vec2 f = fract(st);

    // Four corners in 2D of a tile
    float a = N21(i);
    float b = N21(i + vec2(1.0, 0.0));
    float c = N21(i + vec2(0.0, 1.0));
    float d = N21(i + vec2(1.0, 1.0));

    // Smooth Interpolation

    // Cubic Hermine Curve.  Same as SmoothStep()
    vec2 u = f*f*(3.0-2.0*f);

    // Mix 4 coorners percentages
    return mix(a, b, u.x) +
            (c - a)* u.y * (1.0 - u.x) +
            (d - b) * u.x * u.y;
}

float perlin2(vec2 uv, int octaves, float pscale){
    float col = 1.;
    float initScale = 4.;  
    for ( int l; l < octaves; l++){
        float val = noise(uv*initScale);
        if (col <= 0.01){
            col = 0.;
            break;
        }
        val -= 0.01;
        val *= 0.5;
        col *= val;
        initScale *= pscale;
    }
 	return col;
}

vec3 stars(vec2 uv, float offset){
    
    float timeScale = -offset / layers;
    
    float trans = fract(timeScale);
    
    vec3 col = vec3(0.);
   
    
    // translate uv then scale for center
    uv -= vec2(0.5);
    uv = scale( vec2(trans) ) * uv;
    uv += vec2(0.5);
    
    // create square aspect ratio
    uv.x *= iResolution.x / iResolution.y;
    
    // add nebula colours
    float colR = N21(vec2(offset));
    float colB = N21(vec2(offset*123.));
    
    // generate perlin noise nebula on every third layer
    if (mod(offset,3.) == 0.){
    	float perl = perlin2(uv+offset,3,2.);
    	col += vec3(perl,perl,perl) * 0.001;
    }
    
    // create boxes
    uv *= repeats;
    
    // get position
    vec2 ipos = floor(uv);
    
    // return uv as 0 to 1
    uv = fract(uv);
    
    // calculate random xy and size
    vec2 rndXY = N22(ipos*(offset+1.))*0.9+0.05;
    float rndSize = N21(ipos)*100.+200.;
    
    
    vec2 j = (rndXY - uv)*rndSize;
    float sparkle = 1./dot(j,j);
    
    col += spectrum(fract(rndXY*ipos)) * vec3(sparkle);
    
    
	// visualize layers
    /*if ((uv.x > 9. || uv.y > 0.99) && ipos.y == 8.){
        col += vec3(1.,0.,0.)*smoothstep(1.,0.5,trans);
    }
    if (mod(offset,3.) == 0.){
    	if (uv.x > 0.99 || uv.y > 0.99){
        	col += vec3(1.,0.,0.)*smoothstep(0.2,0.1,trans);
    	}
    }*/
    
   	col *= smoothstep(1.,0.8,trans);	
    col *= smoothstep(0.,0.1,trans);
    return col;
}

vec4 starfield(vec2 fragCoord)
{
    // Normalized pixel coordinates (from 0 to 1)
    vec2 uv = fragCoord/iResolution.xy;
    
    vec3 col = vec3(0.);
	
    for (float i = 0.; i < layers; i++ ){
    	col += stars(uv, i);
    }

    // Output to screen
    return vec4(col,1.0);
}

void main() {
    vec2 uv = (fragUV * 2.0 - 1.0);  // [-1,1] range
    uv.x *= iResolution.x / iResolution.y;  // maintain aspect ratio

    vec3 lightPosition = vec3(0.0, 0.0, -5.0);
    vec3 spherePosition = vec3(0.0, 0.0, 0.0);
    float sphereRadius = 1.8;
    vec3 cameraPosition = vec3(0.0, 0.0, -10.0);

    vec3 pixelPosition = vec3(uv.x / 5.0, uv.y / 5.0, -9.0);
    vec3 ray = normalize(pixelPosition - cameraPosition);

    mat3 rot = xrot(radians(-iLat)) * yrot(radians(iLon));
    ray = ray * rot;
    cameraPosition = cameraPosition * rot;

    float distance = intersectSphere(cameraPosition, ray, spherePosition, sphereRadius);

    if (distance > 0.0) {
        vec3 pointOfIntersection = cameraPosition + ray * distance;
        vec3 normal = normalize(pointOfIntersection - spherePosition);

        float lon = atan(normal.z, normal.x);
        float lat = asin(normal.y);

        const float pi = 3.1415926;
        const float twopi = 2.0 * pi;

        float ulon = lon / twopi;
        float ulat = lat / pi;

        float u = 0.5 + ulon;
        float v = 0.5 + ulat;

        float brightness = dot(normalize(lightPosition - spherePosition), normal);
        brightness = max(brightness, 0.0);

        vec4 outputColor = texture(iChannel0, vec2(fract(u), fract(v)));

        // Compute closest latitude/longitude gridlines
        float glon = floor(ulon * 18.0 + 0.5) * twopi / 18.0;
        float glat = floor(ulat * 10.0 + 0.5) * pi / 10.0;

        float du = abs(dot(normal.xz, vec2(-sin(glon), cos(glon))));
        float dv = abs(glat - lat);
        float dmin = min(du, dv);

        // Smoothstep-based antialiasing on gridlines
        // outputColor *= 0.5 + 0.5 * smoothstep(0.01, 0.02, dmin);

        fragColor = outputColor;// * brightness;
    } else {
        fragColor = starfield(gl_FragCoord.xy);
    }
}
