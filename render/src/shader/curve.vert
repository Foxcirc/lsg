
// This is the vertex shader for the curve renderer.
// The vertex shader is the same for every kind of triangle.

#version 320 es
// #version 440 core
precision mediump float;

layout (location = 0) in vec2 pos; // TODO: remove location = N and use gl::attrib_location(name)
layout (location = 1) in vec2 uvIn;

out vec2 uv;

void main() {
    uv = uvIn; // the uv's will be interpolated for the fragment shader
    gl_Position = vec4(pos.x, pos.y, 0.0, 1.0);
}
