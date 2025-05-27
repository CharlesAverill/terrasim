#version 460 core

layout (location = 0) in vec2 aPos;        // vertex of quad
layout (location = 1) in vec3 aDirection;  // unit vector pointing to tile center
layout (location = 2) in vec3 aColor;

out vec3 Color;

uniform float uRadius;         // sphere radius in NDC (0..1)
uniform float uRotLon;         // rotation longitude in radians
uniform float uRotLat;         // rotation latitude in radians

// Apply 3D rotation matrix
vec3 rotate(vec3 p, float lat, float lon) {
    float sinLat = sin(lat), cosLat = cos(lat);
    float sinLon = sin(lon), cosLon = cos(lon);
    mat3 rot =
        mat3(
            cosLon, sinLat*sinLon, cosLat*sinLon,
            0.0,   cosLat,        -sinLat,
           -sinLon, sinLat*cosLon, cosLat*cosLon
        );
    return rot * p;
}

void main() {
    vec3 p = rotate(aDirection, uRotLat, uRotLon);
    if (p.z < 0.0) {
        gl_Position = vec4(-2.0, -2.0, -1.0, 1.0); // discard off-sphere
    } else {
        vec2 center = p.xy * uRadius;
        gl_Position = vec4(center + aPos * 0.05, 0.0, 1.0); // a small quad around center
        Color = aColor;
    }
}
