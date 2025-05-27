#version 460 core
// aPos is our input vector
layout (location = 0) in vec3 aPos;

/*
    A vertex shader converts vertex coords to normalized device coordinates,
    which indicate to OpenGL where to put them on the screen.
*/

void main()
{
    // gl_Position is the output of the vertex shader
    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
}
