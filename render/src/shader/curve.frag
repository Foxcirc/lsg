
#version 320 es
precision mediump float;

in vec2 curvePos;
in vec3 texture;

layout (location = 0) out vec4 outColor;
layout (location = 1) out float outReveal;

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

        vec4 color = vec4(texture, 0.5);

        // float weight = max(min(1.0, max(max(color.r, color.g), color.b) * color.a), color.a) *
        //     clamp(0.03 / (1e-5 + pow(gl_FragCoord.z / 200.0, 4.0)), 1e-2, 3e3);

        float weight = color.a / (0.1 + pow(gl_FragCoord.z, 2.0));

        outColor  = vec4(color.rgb * color.a * weight, color.a);
        outReveal = color.a * weight;

        // outColor = color;

    } else {
        discard;
    }

}
