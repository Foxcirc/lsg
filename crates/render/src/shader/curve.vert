#version 300 es
precision mediump float;

// For the description of the vertex layout see comments in the gl-renderer (fn prepare).
layout (location = 0) in uint inFlags;
layout (location = 1) in ivec2 inXY;
layout (location = 2) in uvec2 inTEX;

out vec4 textureData; // color/tex-coords, depending on isAtlas
out vec2 curvePosition;
out vec3 barycentric;
flat out uint fillKind;
flat out uint isAtlas;

const vec2 curvePositionTable[9] = vec2[](
    vec2(0.0, 1.0), vec2(0.0, 1.0),  vec2(0.0, 1.0),  // FILLED
    vec2(0.0, 0.0), vec2(0.5, 0.0),  vec2(1.0, 1.0),  // CONVEX
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
    uint inIsAtlas   = (inFlags >> 8u) & 1u; // 1 bit

    fillKind = inFillKind;
    isAtlas  = inIsAtlas;

    uvec4 maybeColor = uvec4(
        (inTEX.x >> 0u) & 0xFFu, // r
        (inTEX.x >> 8u) & 0xFFu, // g
        (inTEX.y >> 0u) & 0xFFu, // b
        (inTEX.y >> 8u) & 0xFFu  // a
    );

    // Convert coordinates to OpenGL's form.

    if (inIsAtlas == 1u) {
        textureData = vec4(
            (float(inTEX.x) + 2500.0) / 5000.0,
            (float(inTEX.y) + 2500.0) / 5000.0,
            0.0, 0.0 // unused
        );
    } else {
        textureData = vec4(
            float(maybeColor.r) / 255.0,
            float(maybeColor.g) / 255.0,
            float(maybeColor.b) / 255.0,
            float(maybeColor.a) / 255.0
        );
    }

    gl_Position = vec4(
        float(inXY.x) / 2500.0,
        float(inXY.y) / 2500.0,
        0.0, 1.0 // unused coords
    );

    curvePosition = lookupCurvePosition(fillKind, vertexIndex);

    // Handle information for anti-aliasing.

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
