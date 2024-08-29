
```rust

struct Geometry {
	points: Vec<Point>,
	polygons: Vec<Polygon>, // specifies which points form a polygon
	instances: Vec<Instance>,
}

struct Polygon {
	range: Range, // index into `points`
	instanced: bool,
}

struct Point {
	x: u16, y: u16,
}

struct Vertex {
	x: f32, y: f32,
	cx: f32, cy: f32,
	// tx: f32, ty: f32, texture: f32,
}

struct Instance {
	idx: usize, // index into polygons
	offset: Vec2,
	texture: handle,
}

fn draw(...) {

	out.single(shape, offset, color); // has specific color, etc.

	let handle = out.cache(shape); // create an instance
	out.cached(handle, offset, color); // has specific color, etc.
	
}

```
