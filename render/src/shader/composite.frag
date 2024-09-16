
#version 320 es
precision mediump float;

uniform sampler2D colorTexture;
uniform sampler2D revealTexture;

// in vec2 texCoords;
out vec4 outColor;

void main() {
    // TODO: should we use texCoords or does this work (with the gl_FragCoord thing)
    // TODO: update this slightly http://casual-effects.blogspot.com/2015/03/implemented-weighted-blended-order.html
    vec4 accum = texelFetch(colorTexture, ivec2(gl_FragCoord.xy), 0);
    float r = accum.a;
    accum.a = texelFetch(revealTexture, ivec2(gl_FragCoord.xy), 0).r;
    outColor = vec4(accum.rgb / clamp(accum.a, 1e-4, 5e4), r);
}
