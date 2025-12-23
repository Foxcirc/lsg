#version 320 es
precision mediump float;

in vec2 curvePosition;
in vec3 textureCoords;
in vec3 barycentric;
in flat uint fillKind;

out vec4 color;

void main() {

    float value = curvePosition.y - pow(curvePosition.x, 2.0) * sign(curvePosition.x);
    float multiplier;

    if (value < 0.0) {

        // This is where the curve cutout happens.
        multiplier = 0.0;

    } else {

        //  Anti-Aliasing of normal triangles.
        vec3 d = fwidth(barycentric);
        vec3 edgeDist = barycentric / d;
        float dist = min(min(edgeDist.x, edgeDist.y), edgeDist.z);
        // multiplier = clamp(dist, 0.0, 1.0);
        multiplier = 1.0;
    }

    float alpha = 1.0;
    color = vec4(textureCoords.rgb, alpha * multiplier);

}
