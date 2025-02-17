
#version 320 es
precision mediump float;

in vec2 curvePos;
in vec3 texture;

out vec4 Color;

void main() {

    float curveX = curvePos.x;
    float curveY = curvePos.y;

    // calculate if this shape is even part of a curve
    bool isCurve = curveX != 2.0;

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

    // magic curve rescaling equation
    float value = curveY - pow(curveX, 2.0);
    if (!convex) {
        value = -value; // invert what we are filling for concave curves
    }

    float threshold = 0.002;
    float smoothness = 0.008;
    float multiplier;
    if ((value < threshold) && isCurve) {
        multiplier = smoothstep(threshold - smoothness, threshold + smoothness, value);
    } else {
        multiplier = 1.0;
    }

    float alpha = 1.0; // TODO: add alpha channel to texture
    Color = vec4(texture, alpha * multiplier);


}
