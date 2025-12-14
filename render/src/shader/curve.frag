
#version 320 es
precision mediump float;

in vec2 curvePos;
in vec3 texture;
in vec3 barycentric;

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
    if (isCurve) {
        if (value < 0.0) {
            multiplier = 0.0;
        } else {

            float width = fwidth(value);
            float coverage = smoothstep(0.0, width, value);

            multiplier = coverage;

        }
    } else {
        vec3 d = fwidth(barycentric);
        vec3 edgeDist = barycentric / d;
        float dist = min(min(edgeDist.x, edgeDist.y), edgeDist.z);
        // multiplier = smoothstep(0.0, width, edgeDistance);
        multiplier = clamp(dist, 0.0, 1.0);
    }

    float alpha = 1.0; // TODO: add alpha channel to texture
    Color = vec4(texture.rgb, alpha * multiplier);

}
