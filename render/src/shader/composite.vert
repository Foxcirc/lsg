
#version 320 es
precision mediump float;

layout(location = 0) in vec2 inPosition;
out vec2 texCoords;

void main() {
    texCoords = inPosition * 0.5 + 0.5;  // convert [-1, 1] range to [0, 1] range
    gl_Position = vec4(inPosition, 0.0, 1.0);
}
