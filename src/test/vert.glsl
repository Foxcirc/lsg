#version 320 es

// precision mediump float;
// precision mediump int;

layout (location = 0) in vec3 aPos;

void main()
{
    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
}

// #version 330 core

// layout (location = 0) in vec3 pos;

// void main() {
//     gl_Position = vec4(pos.x, pos.y, pos.z, 1.0);
// }

