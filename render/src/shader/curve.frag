
#version 320 es
precision mediump float;

in vec2 curvePos;
in vec3 texture;

out vec4 final;

void main() {

    // extract convexity information out of the curvePos

    float curveX = curvePos.x;
    float curveY = curvePos.y;

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

    bool inside = convex
        ? curveY >= pow(curveX, 2.0)
        : curveY <= pow(curveX, 2.0);

    if (inside) {
        final = vec4(texture, 1.0);
    } else {
        discard;
    }

    // final = inside
    //     ? vec4(texture, 1.0)
    //     : vec4(0.0, 0.0, 0.0, 0.0);

}
