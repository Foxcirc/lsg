
#version 320 es
precision mediump float;

in vec2 curvePos;
in vec3 texture;

out vec4 Color;

void main() {

    float curveX = curvePos.x;
    float curveY = curvePos.y;

    // calculate if this shape is even a curve-triangle
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

    // decide if we are inside the curve
    float value = curveY - pow(curveX, 2.0);
    if (!convex) { // invert what we are filling for concave curves
        value = -value;
    }

    float multiplier;
    if (value < 0.0 && isCurve) { // outside the curve (completely transparent)
        multiplier = 0.0;
    } else if (value < 1.0 / 500.0 && isCurve) { // anti-aliasing
        multiplier = 0.5;
    } else if (value < 3.0 / 500.0 && isCurve) { // anti-aliasing
        multiplier = 0.8;
    } else { // inside the curuve
        multiplier = 1.0;
    }

    float red = texture.r;
    if (isCurve) {
        red = curveX;
    }

    float alpha = 1.0; // TODO: add alpha channel to texture
    Color = vec4(red, texture.gb, alpha * multiplier);

}
