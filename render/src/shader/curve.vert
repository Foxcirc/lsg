
#version 320 es
precision mediump float;

layout (location = 0) in vec3 posIn;
layout (location = 1) in vec2 curvePosIn;
layout (location = 2) in vec3 textureIn;

layout (location = 3) in vec3 iPosIn;
layout (location = 4) in vec3 iTextureIn;
 
out vec2 curvePos;
out vec3 texture;

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

}
