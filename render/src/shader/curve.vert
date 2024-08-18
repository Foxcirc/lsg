
# version 320 es
precision mediump float;

layout (location = 0) in vec3 pos;
layout (location = 1) in vec2 uvIn; // todo: try inout keyword instead of in

out vec2 uv;

void main() {
    gl_Position = vec4(pos.x, pos.y, pos.z, 1.0);
    uv = uvIn;
}
