
// This is the fragment shader for the curve renderer.
// It uses a uniform to select the specific kind of triangles that should
// be expected.
// - completely filled
// - convex curved
// - concave curved

#version 320 es
// #version 440 core
precision mediump float;

#define FILLED  1u
#define CONVEX  2u
#define CONCAVE 3u

uniform uint mode;

in vec2 uv;
out vec4 final;

void main() {

    // float r = float(int(uv.x * 10.0)) / 10.0;
    // float g = float(int(uv.y * 10.0)) / 10.0;
    // float b;
    float a;
    float b;

    if      (mode == FILLED)  { a = 1.0; b = 0.0; }
    else if (mode == CONVEX)  { a = float(uv.y > uv.x * uv.x); b = 0.4; }
    else if (mode == CONCAVE) { a = float(uv.y < uv.x * uv.x); b = 0.8; }
    
    a = min(1.0, a + 0.1);

    final = vec4(0.1, 0.5, b, a);

}
