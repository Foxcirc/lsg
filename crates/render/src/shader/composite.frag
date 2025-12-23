
#version 320 es
precision mediump float;

uniform sampler2D texture;
out vec4 Color;

void main() {
    Color = texelFetch(texture, ivec2(gl_FragCoord.xy), 0);
}
