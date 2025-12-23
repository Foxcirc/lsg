#version 320 es
precision mediump float;

// For the description of the vertex layout see comments in the gl-renderer.
layout (location = 0) in uint inFlags;
layout (location = 1) in uint inXYZ;
layout (location = 2) in uint inUVL;

out vec2 curvePosition;
out vec3 textureCoords;
out vec3 barycentric;
out flat uint fillKind;

const vec2 curvePositionTable[9] = vec2[](
    vec2(0.0, 1.0), vec2(0.0, 1.0),  vec2(0.0, 1.0),  // FILLED
    vec2(0.0, 0.0), vec2(0.5, 0.0), vec2(1.0, 1.0),   // CONVEX
    vec2(0.0, 0.0), vec2(-0.5, 0.0), vec2(-1.0, -1.0) // CONCAVE
);

vec2 lookupCurvePosition(uint row, uint col) {
    return curvePositionTable[row * 3u + col];
}

void main() {

    // Unpack our inputs.

    uint outerEdges  = (inFlags >> 0u) & 7u; // 3 bit
    uint vertexIndex = (inFlags >> 3u) & 3u; // 2 bit
    uint instanced   = (inFlags >> 5u) & 1u; // 1 bit
    uint inFillKind  = (inFlags >> 6u) & 3u; // 2 bit

    fillKind = inFillKind;

    uvec3 uintXYZ = uvec3(
        (inXYZ >> 20u) & 4095u, // X, 12 bit
        (inXYZ >> 8u)  & 4095u, // Y, 12 bit
        (inXYZ >> 0u)  & 255u   // Z, 8  bit
    );

    // Convert coordinates to NDC form.

    vec3 xyz = vec3(
        float(int(uintXYZ.x) - 2048) / 2048.0,
        float(int(uintXYZ.y) - 2048) / -2048.0, // INVERTED!
        float(uintXYZ.z) / 255.0
    );

    curvePosition = lookupCurvePosition(fillKind, vertexIndex);
    textureCoords = vec3(1.0);
    gl_Position = vec4(xyz.x, xyz.y, 0.0, 1.0);

    bool edgeABisOuter = ((outerEdges >> 2) & 1u) == 1u;
    bool edgeBCisOuter = ((outerEdges >> 1) & 1u) == 1u;
    bool edgeCAisOuter = ((outerEdges >> 0) & 1u) == 1u;

    barycentric = vec3(0.0, 0.0, 0.0);

    if (!edgeABisOuter) barycentric.z = 0.5;
    if (!edgeBCisOuter) barycentric.x = 0.5;
    if (!edgeCAisOuter) barycentric.y = 0.5;

    switch (vertexIndex) {
        case 0u: barycentric.x = 1.0; break;
        case 1u: barycentric.y = 1.0; break;
        case 2u: barycentric.z = 1.0; break;
    }

}
