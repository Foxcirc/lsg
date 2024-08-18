
# version 320 es
precision mediump float;

// we use the uv's to check if we are inside
// the curve or not
in vec2 uv;

out vec4 final;

void main() {

    // todo: flip inside/outside condition based on direction or smth

    float r = float(int(uv.x * 10.0)) / 10.0;
    float g = float(int(uv.y * 10.0)) / 10.0;
    float b = float(uv.y > uv.x * uv.x);

    final = vec4(r, g, b, 1.0);

}
