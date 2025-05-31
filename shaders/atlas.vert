#version 460 core

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aOffset;
layout (location = 2) in vec3 aColor;

out vec3 Color;

uniform float uScaleX;
uniform float uScaleY;

void main() {
    gl_Position = vec4(vec2(aPos.x * uScaleX, aPos.y * uScaleY) + aOffset, 0.0, 1.0);
    Color = aColor;
}
