#version 320 es

// precision mediump float;
// precision mediump int;

in vec3 vertexPos;
in vec3 vertexColor;

out vec3 outColor;

void main()
{
    gl_Position = vec4(vertexPos, 1.0);
    outColor = vertexColor;
}
