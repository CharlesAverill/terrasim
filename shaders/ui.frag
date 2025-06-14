#version 460 core

in vec2 TexCoord;
out vec4 FragColor;

uniform sampler2D u_texture;

void main() {
    FragColor = texture(u_texture, TexCoord);
}
