#version 320 es
precision mediump float;

in vec2 curvePosition;
in vec3 textureCoords;
in vec3 barycentric;
in flat uint fillKind;

out vec4 color;

void main() {

    float value = curvePosition.y - pow(curvePosition.x, 2.0) * sign(curvePosition.x);

    // switch (fillKind) {
    //     case 0u: value = 1.0; break;
    //     case 1u: value =   curvePosition.y - pow(curvePosition.x, 2.0);  break;
    //     case 2u: value = -(curvePosition.y - pow(curvePosition.x, 2.0)); break;
    // }

    bool isCurve = fillKind != 0u;

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

    float alpha = 1.0;
    color = vec4(textureCoords.rgb, alpha * multiplier);

}
