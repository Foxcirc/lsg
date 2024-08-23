
// This is the fragment shader for the curve renderer.
// It uses a uniform to select the specific kind of triangles that should
// be expected.
// - completely filled
// - convex curved
// - concave curved

#version 320 es
// #version 440 core
precision mediump float;

// #define FILLED  1u
// #define CONVEX  2u
// #define CONCAVE 3u

// uniform uint mode;

in vec2 uv;
out vec4 final;

void main() {

    float r = float(int(uv.x * 10.0)) / 10.0;
    float g = float(int(uv.y * 10.0)) / 10.0;
    float b;
    float a;

    // if      (mode == FILLED)  { a = 1.0; }
    // else if (mode == CONVEX)  { a = float(uv.y > uv.x * uv.x); }
    // else if (mode == CONCAVE) { a = float(uv.y < uv.x * uv.x); }

    a = float(uv.y > uv.x * uv.x);
    b = float(a == 1.0);
    final = vec4(0.2, g, b, 1.0);

    // if (uv.x != uv.y) {
    //     final = vec4(float(mode) / 3.0, 1.0, 0.0, 1.0);
    // }
}
