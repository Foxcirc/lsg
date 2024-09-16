
#version 320 es
precision mediump float;

uniform sampler2D colorTexture;
uniform sampler2D revealTexture;

in vec2 texCoords;
out vec4 outColor;

void main() {

    vec4  accum  = texelFetch(colorTexture,  ivec2(gl_FragCoord.xy), 0);
    float reveal = texelFetch(revealTexture, ivec2(gl_FragCoord.xy), 0).r;

    outColor = vec4(accum.rgb / max(accum.a, 1e-5), reveal);

}
