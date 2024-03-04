#version 320 es

precision highp float;
precision highp int;

in vec3 outColor;

out vec4 fragmentColor;

void main() {
    fragmentColor = vec4(outColor.x, 1.0, 1.0, 1.0);
}
