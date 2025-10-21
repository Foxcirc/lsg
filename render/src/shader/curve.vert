
#version 320 es
precision mediump float;

layout (location = 0) in vec3 posIn;
layout (location = 1) in vec2 curvePosIn;
layout (location = 2) in vec3 textureIn;
layout (location = 3) in uint flagsIn; // TODO: document

layout (location = 4) in vec3 iPosIn;
layout (location = 5) in vec3 iTextureIn;

out vec2 curvePos;
out vec3 texture;
out vec3 barycentric;

void main() {

    if (iPosIn == vec3(-1.0) && iTextureIn == vec3(-1.0)) { // this is not instanced

        curvePos = curvePosIn; // we only need the interpolation
        texture = textureIn; // TODO: select the layer here and pass it along as a 2d texture
        gl_Position = vec4(posIn.x, posIn.y, posIn.z, 1.0); // positioning is already done

    } else { // this is instanced

        curvePos = curvePosIn; // this is always present
        texture = iTextureIn; // TODO: we need to interpolate the texture along the polygon here for instanced shapes
        gl_Position = vec4(posIn.x + iPosIn.x, posIn.y + iPosIn.y, iPosIn.z, 1.0); // add the relevant offset

    }

    bool edgeABisOuter = ((flagsIn >> 0) & 1u) == 1u;
    bool edgeBCisOuter = ((flagsIn >> 1) & 1u) == 1u;
    bool edgeACisOuter = ((flagsIn >> 2) & 1u) == 1u;

    barycentric = vec3(0.0, 0.0, 0.0);

    // the barycentrics are used inside the fragment shader
    // to do antialiasing for filled triangles
    // if (edgeKind != 0.0) { // edgeKind == 0.0 means it is an inside edge
        if (curvePosIn.x == 2.0) { // first vertex of the triangle
            barycentric.x = 1.0;
            if (!edgeABisOuter) barycentric.z = 0.5;
            if (!edgeACisOuter) barycentric.y = 0.5;
            curvePos = vec2(2.0);
        } else if (curvePosIn.x == 3.0) { // second vertex of the triangle
            barycentric.y = 1.0;
            if (!edgeABisOuter) barycentric.z = 0.5;
            if (!edgeBCisOuter) barycentric.x = 0.5;
            curvePos = vec2(2.0);
        } else if (curvePosIn.x == 4.0) { // third vertex of the triangle
            barycentric.z = 1.0;
            if (!edgeBCisOuter) barycentric.x = 0.5;
            if (!edgeACisOuter) barycentric.y = 0.5;
            curvePos = vec2(2.0);
        };
    // } else {
    //     barycentric == vec3(0.0);
    // }

}
