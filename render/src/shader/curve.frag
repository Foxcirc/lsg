
#version 320 es
precision mediump float;

in vec2 curvePos;
in vec3 texture;

out vec4 Color;

void main() {

    float curveX = curvePos.x;
    float curveY = curvePos.y;

    // extract convexity information out of the curvePos
    // and convert curvePos into range 0..1

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

    float value = curveY - pow(curveX, 2.0);
    if (!convex) {
        value = -value; // invert what we are filling for concave curves
    }

    if (value >= 0.0) {
        vec3 v;
        if (curveX == 1.0) {
            v = texture;
        } else {
            // v = vec3(float(int(curveX * 10.0) / 10), curveY, 0.0);
            v = texture;
        }
        Color = vec4(v, 1.0);
    } else {
        discard;
    }

}
