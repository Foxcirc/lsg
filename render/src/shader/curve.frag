
#version 320 es
precision mediump float;

in vec2 uv;
out vec4 final;

void main() {

    // extract convexity information out of the uv's

    float curveX = uv.x;
    float curveY = uv.y;

    bool convex;
    if (curveX > 0.5) {
        curveX = (curveX - 0.5) * 2.0;
        curveY = (curveY - 0.5) * 2.0;
        convex = true;
    } else {
        curveX = curveX * 2.0;
        curveY = curveY * 2.0;
        convex = false;
    }

    bool inside;
    if (convex) { // convex
        inside = curveY >= pow(curveX, 2.0);
    } else { // concave
        inside = curveY <= pow(curveX, 2.0);
    }

    final = vec4(0.3, 0.0, 0.8, float(inside));

}
