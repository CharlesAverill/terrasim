#version 460 core
// https://www.shadertoy.com/view/Ml2XRV#

in vec2 fragUV;
out vec4 fragColor;

uniform int iTime;
uniform vec2 iResolution;   // Screen resolution
uniform sampler2D iChannel0; // Input texture (tilemap/biome/etc.)
uniform int iLat;  // latitude rotation in radians
uniform int iLon;  // longitude rotation in radians

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
        // Starfield: use UV as seed to sample pseudo-random stars
        vec2 uv = fragUV;
        vec2 seed = floor(uv * iResolution.xy / 2.0);  // coarsen grid for stars
        float noise = fract(sin(dot(seed ,vec2(12.9898,78.233))) * 43758.5453);

        float star = smoothstep(0.995, 1.0, noise);  // threshold for stars
        vec3 starColor = vec3(star);

        fragColor = vec4(starColor, 1.0);
    }
}
