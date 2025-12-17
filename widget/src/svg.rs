
use std::{f64::consts::PI, iter::once, mem};

use common::*;

#[derive(Debug)]
pub enum PathCommand {
    Move(Point),
    Line(Point),
    Horizontal(f32),
    Vertical(f32),
    Quadratic(Point, Point),
    Cubic(Point, Point, Point),
    Return,
}

pub mod parser {

    use super::PathCommand;

    use common::*;
    use nom::{
        IResult, Parser,
        branch::alt,
        bytes::complete::tag,
        character::complete::{char, space0, digit1},
        combinator::{map, map_res, opt, recognize, success},
        multi::many0,
        sequence::preceded,
    };

    pub fn path(text: &str) -> IResult<&str, Vec<PathCommand>> {
        many0(command).parse(text)
    }

    fn command(text: &str) -> IResult<&str, PathCommand> {
        preceded(
            space0,
            alt((
                preceded(tag("M"), map(point, |p1| PathCommand::Move(p1))),
                preceded(tag("H"), map(coord, |v1| PathCommand::Horizontal(v1))),
                preceded(tag("V"), map(coord, |v1| PathCommand::Vertical(v1))),
                preceded(tag("L"), map(point, |p1| PathCommand::Line(p1))),
                preceded(tag("Q"), map((point, point), |(p1, p2)| PathCommand::Quadratic(p1, p2))),
                preceded(tag("C"), map((point, point, point), |(p1, p2, p3)| PathCommand::Cubic(p1, p2, p3))),
                preceded(tag("Z"), map(success(()), |()| PathCommand::Return)),
            ))
        ).parse(text)
    }

    fn point(text: &str) -> IResult<&str, Point> {
        map((
            coord, coord
        ), |(v1, v2)| Point::new(v1, v2)
        ).parse(text)
    }

    fn coord(text: &str) -> IResult<&str, f32> {
        map_res(
            preceded(space0, recognize((digit1, opt((char('.'), digit1))))),
            |val: &str| val.parse::<f32>()
        ).parse(text)
    }

}

// TODO: reduce transient allocations

#[derive(Debug, Default, PartialEq, Eq)]
enum SectionDirection {
    #[default]
    Unknown,
    Cw,
    Ccw,
}

#[derive(Debug, Default)]
struct Section {
    pub points: Vec<CurvePoint>,
    pub area: usize,
    pub direction: SectionDirection,
    pub level: usize,
    pub group: usize,
}

impl Section {
    pub fn should_be_filled(&self) -> bool {
        self.level % 2 == 0
    }
    pub fn should_be_reversed(&self) -> bool {
        let fill = self.should_be_filled();
         fill && self.direction == SectionDirection::Cw ||
        !fill && self.direction == SectionDirection::Ccw
    }
}

pub fn path_to_shape(path: Vec<PathCommand>, scale: f32) -> Vec<Vec<CurvePoint>> {

    // Step 1:
    // Convert commands to list of points and split the path into sub-sections that need to be
    // connected by an invisible line. Holes or filled regions inside holes are considered sub-sections.

    let mut sections: Vec<Section> = Vec::with_capacity(4);
    let mut current: Vec<CurvePoint> = Vec::with_capacity(24);
    let mut start = Point::default();
    let mut cursor = Point::default();

    for command in path {

        match command {
            PathCommand::Move(p1) => {
                // create a new sub area, if we already collected some points
                if current.len() > 0 {
                    sections.push(Section {
                        points: mem::take(&mut current),
                        ..Default::default()
                    });
                }
                // add the point
                current.push(CurvePoint::convert(p1 * scale, PointKind::Base));
                start = p1;
                cursor = p1;
            },
            PathCommand::Line(p1) => {
                current.push(CurvePoint::convert(p1 * scale, PointKind::Base));
                cursor = p1;
            },
            PathCommand::Horizontal(pos) => {
                current.push(CurvePoint::convert(Point::new(pos, cursor.y) * scale, PointKind::Base));
                cursor.x = pos;
            },
            PathCommand::Vertical(pos) => {
                current.push(CurvePoint::convert(Point::new(cursor.x, pos) * scale, PointKind::Base));
                cursor.y = pos;
            },
            PathCommand::Quadratic(ct, p1) => {
                current.push(CurvePoint::convert(ct * scale, PointKind::Ctrl));
                current.push(CurvePoint::convert(p1 * scale, PointKind::Base));
                cursor = p1;
            },
            PathCommand::Cubic(c1, c2, p1) => {
                current.push(CurvePoint::convert(c1 * scale, PointKind::Ctrl));
                current.push(CurvePoint::convert(c2 * scale, PointKind::Ctrl));
                current.push(CurvePoint::convert(p1 * scale, PointKind::Base));
                cursor = p1;
            },
            PathCommand::Return => {
                current.push(CurvePoint::convert(start * scale, PointKind::Base));
                cursor = start;
            },
        }

    }

    // The last section won't have a move command after it,
    // to trigger the push, so we push it here.
    current.dedup();
    sections.push(Section {
        points: current,
        ..Default::default()
    });

    // Step 2:
    // Compute signed area and direction (cw/ccw). This is used to
    // decide which regions should be filled in the third step.

    for section in &mut sections {

        let len = section.points.len();
        let points = &section.points;
        let mut shoelace = 0;
        for idx in 0..len {
            shoelace += (points[idx].x() as isize * points[(idx + 1) % len].y() as isize) -
                        (points[idx].y() as isize * points[(idx + 1) % len].x() as isize);
        }

        match shoelace >= 0 {
            true => section.direction = SectionDirection::Cw,
            false => section.direction = SectionDirection::Ccw,
        }

        section.area = shoelace.abs() as usize;

    }

    // Step 3:
    // Determine which sections are filled and which are empty.
    // We do this by checking how many sections are completely outside of the section
    // identified by `outer_idx`.

    for outer_idx in 0..sections.len() {

        // the number of sections the `outer_idx` section is contained inside of
        let mut parents = 0;

        for inner_idx in 0..sections.len() {

            if outer_idx == inner_idx { continue };

            // we only need to check one point, since we assume no self-intersections
            let point = sections[outer_idx].points[0];
            let polygon = &sections[inner_idx].points;

            if point_inside_polygon(point, polygon) {
                parents += 1;
            }

        }

        // we fill using the even-odd rule, for now
        sections[outer_idx].level = parents;

        // some sections will need to be reversed to be filled correctly by the renderer since
        // in lsg's world:
        // left-winding = filled
        // right-winding = hole

        if sections[outer_idx].should_be_reversed() {
            sections[outer_idx].points.reverse();
        }

    }

    // Step 4:
    // Group sections together based on their level and which other sections
    // they contain, so that every group contains a filled section and its holes.

    let mut removed: Vec<bool> = Vec::new();
    removed.resize(sections.len(), false);

    let mut group_counter = 0;
    let mut outer_idx = 0;
    while outer_idx < sections.len() {

        let inner_level = sections[outer_idx].level + 1;

        let fill = sections[outer_idx].should_be_filled();
        if fill && !removed[outer_idx] {

            // Now put all sections at level - 1 that are inside
            // the outer section into the same group.

            sections[outer_idx].group = group_counter;

            let mut inner_idx = 0;
            while inner_idx < sections.len() {

                let fill = sections[inner_idx].should_be_filled();
                if !fill && !removed[inner_idx] {

                    let point = sections[inner_idx].points[0];
                    let polygon = &sections[outer_idx].points;

                    if sections[inner_idx].level == inner_level &&
                       point_inside_polygon(point, polygon)
                    {
                        sections[inner_idx].group = group_counter;
                        removed[inner_idx] = true;
                    }

                }

                inner_idx += 1;

            }

            group_counter += 1;

        }

        outer_idx += 1;

    }

    // Step 5:
    // Form one shape for each group, by connecting the filled parent to its holes.

    let mut shapes: Vec<Vec<CurvePoint>> = Vec::new();

    for outer in sections.iter() {

        if outer.should_be_filled() {

            let mut shape: Vec<CurvePoint> = Vec::new();
            let mut spots: Vec<IndexedConnectionSpot> = Vec::new();

            // Find the connection spots.

            for (idx, inner) in sections.iter().enumerate() {

                if inner.group == outer.group &&
                   inner.level != outer.level {

                    let spot = find_connection_spot_for_group(
                        // We only try to find connections between the outer polygon and a hole polygon but
                        // not between the hole polygons themselves. This is important for the algorithm later.
                        &outer.points, &inner.points,
                        // Check only sections of the current group.
                        &sections, inner.group
                    ).expect("no valid connection spot"); // TODO: for production, make all of this not panic on invalid input but soft-error

                    spots.push(IndexedConnectionSpot {
                        idx,
                        inner: spot,
                    });

                }

            }

            // Sort the spots by the index at which they connect to the outer section.
            spots.sort_unstable_by(|lhs, rhs|
                lhs.inner.lhs.cmp(&rhs.inner.lhs)
            );

            // Create the shape with all points in the correct order, all in a single pass
            let mut start = 0;
            for spot in spots {

                // push all points up to the index of the next connection
                shape.extend_from_slice(&outer.points[start..=spot.inner.lhs]);

                // push the points of the inner hole
                let hole = &sections[spot.idx].points;
                shape.extend_from_slice(&hole[spot.inner.rhs..]);
                shape.extend_from_slice(&hole[..spot.inner.rhs]);

                // push the first point of the hole again, to connect back outerwards later
                shape.push(hole[spot.inner.rhs]);

                // since we are not doing a `+1` here the point at `spot.inner.lhs` will be pushed
                // again next iteration, which closes the loop
                start = spot.inner.lhs;

            }

            // Push the points after the last spot onto the shape
            shape.extend_from_slice(&outer.points[start..]);

            shapes.push(shape);

        }

    }

    shapes

}

// TODO: as part of the never ending rework for math functions taking points/curvepoints this should take an Into<Point> instead of curvepoint
fn point_inside_polygon(point: CurvePoint, polygon: &[CurvePoint]) -> bool {

    let len = polygon.len();

    // the total winding number
    let mut winding = 0.0;

    for idx in 0..len {
        let p1 = polygon[idx];
        let p2 = polygon[(idx + 1) % len];
        let angle1 = (p1.y() as f64 - point.y() as f64).atan2(p1.x() as f64 - point.x() as f64);
        let angle2 = (p2.y() as f64 - point.y() as f64).atan2(p2.x() as f64 - point.x() as f64);
        let mut diff = angle2 - angle1;
        if      diff >  PI { diff -= 2.0 * PI; }
        else if diff < -PI { diff += 2.0 * PI; }
        winding += diff;
    }

    winding.abs() > 1e-5

}

/// Finds indices in `lhs` and `rhs` where a new edge could be created without intersecting any
/// of the edges present in `sections` (considering only sections in `group`).
fn find_connection_spot_for_group(lhs: &[CurvePoint], rhs: &[CurvePoint], sections: &[Section], group: usize) -> Option<ConnectionSpot> {

    // Check every possible edge between the sections.
    'l: for (lhs_point_idx, lhs_point) in lhs.iter().enumerate() {
        'r: for (rhs_point_idx, rhs_point) in rhs.iter().enumerate() {

            let connection_edge = [*lhs_point, *rhs_point].map(Point::from);

            // We can't connect at a control point, since the control point always belongs to
            // it's neightbours.

            if lhs_point.kind() == PointKind::Ctrl {
                continue 'l;
            } else if rhs_point.kind() == PointKind::Ctrl {
                continue 'r;
            };

            // Verify that no other edge intersects this possible connection.
            //
            // We do not need to check for intersections with the infinitely-thin connection strips that
            // were already generated, since these intersections are actually allowed by the later triangulation stages.

            for check_section in sections.iter().filter(|it| it.group == group) {
                for check_edge in pair_windows_wrapping(&check_section.points) {
                    if edges_intersect(connection_edge, check_edge.map(Point::from)) {
                        continue 'r;
                    }
                }
            }

            // Since we didn't  `continue` above no intersections were found and this
            // connection is valid.

            return Some(ConnectionSpot {
                lhs: lhs_point_idx,
                rhs: rhs_point_idx,
            })

        }
    }

    None

}

#[track_caller]
fn pair_windows_wrapping<T: Clone + Copy>(slice: &[T]) -> impl Iterator<Item = [T; 2]> {
    let len =  slice.len();
    let final_pair = [slice[0], slice[len - 1]];
    debug_assert!(len >= 2, "slice must have at least two items");
    slice.windows(2).map(|it| [it[0], it[1]]).chain(once(final_pair))
}

#[derive(Debug, Clone, Copy)]
struct ConnectionSpot {
    pub lhs: usize,
    pub rhs: usize,
}

#[derive(Debug, Clone, Copy)]
struct IndexedConnectionSpot {
    /// The index of the section this belongs to.
    pub idx: usize,
    pub inner: ConnectionSpot,
}

/// This function operates in normal y-space and will return inverted results in inverted y-space.
/// * +1.0 = CW
/// * -1.0 = CCW
/// * 0.0  = colinear
fn three_point_orientation(a: Point, b: Point, c: Point) -> f32 {
    let result = (b.y - a.y) * (c.x - b.x) - (b.x - a.x) * (c.y - b.y);
    result.signum() * (result != 0.0) as u32 as f32
}

/// Simple test for intersection.
/// If the edges overlap on one point that doesn't count as intersecting.
fn edges_intersect(lhs: [Point; 2], rhs: [Point; 2]) -> bool {
    let [a, b] = lhs;
    let [c, d] = rhs;
    let abc = three_point_orientation(a, b, c);
    let abd = three_point_orientation(a, b, d);
    let cda = three_point_orientation(c, d, a);
    let cdb = three_point_orientation(c, d, b);
    abc != abd && cda != cdb &&
    !(abc == 0.0 && !strictly_inside(a, b, c)) &&
    !(abd == 0.0 && !strictly_inside(a, b, d)) &&
    !(cda == 0.0 && !strictly_inside(c, d, a)) &&
    !(cdb == 0.0 && !strictly_inside(c, d, b))
}

/// `false` if the point lies exactly on one of the end points
fn strictly_inside(a: Point, b: Point, q: Point) -> bool {
    a.x.min(b.x) <= q.x && q.x <= a.x.max(b.x) &&
    a.y.min(b.y) <= q.y && q.y <= a.y.max(b.y) &&
    a != q && b != q
}

#[test]
fn test_strictly_inside() {
    let test_cases = [
        ([Point::new(100.0, 500.0), Point::new(200.0, 500.0), Point::new(400.0, 500.0)], false),
        ([Point::new(100.0, 500.0), Point::new(200.0, 500.0), Point::new(200.0, 500.0)], false),
        ([Point::new(100.0, 500.0), Point::new(200.0, 500.0), Point::new(188.0, 500.0)], true),
    ];
    for (input, expected) in test_cases {
        let result = strictly_inside(input[0], input[1], input[2]);
        assert_eq!(expected, result);
    }
}

#[test]
fn test_edges_intersect() {
    let test_cases = [
        ([Point::new(100.0, 500.0), Point::new(300.0, 1800.0), Point::new(300.0, 600.0), Point::new(2100.0, 600.0)], false),
        ([Point::new(100.0, 500.0), Point::new(300.0, 600.0),  Point::new(300.0, 1800.0), Point::new(300.0, 600.0)], false),
        ([Point::new(900.0, 1000.0), Point::new(100.0, 500.0),  Point::new(300.0, 1800.0), Point::new(300.0, 600.0)], true),
    ];
    for (idx, (input, expected)) in test_cases.into_iter().enumerate() {
        let result = edges_intersect([input[0], input[1]], [input[2], input[3]]);
        assert_eq!(expected, result, "test case idx: {}", idx);
    }
}

#[test]
fn test_path_to_shape() {

    // remix icons: arrow up
    const TEST: &str = "M12 2C17.52 2 22 6.48 22 12C22 17.52 17.52 22 12 22C6.48 22 2 17.52 2 12C2 6.48 6.48 2 12 2ZM13 12H16L12 8L8 12H11V16H13V12Z";

    let (_, points) = parser::path(TEST).expect("valid svg path");
    let mut shapes = path_to_shape(points, 416.666);

    assert_eq!(shapes.len(), 1, "should be a single shape");
    let mut shape = shapes.pop().unwrap();

    for point in shape.iter_mut() {
        *point = CurvePoint::new(point.x() / 10, point.y() / 10, PointKind::Base);
    }

    assert_eq!(&shape,
        &[CurvePoint::new(120, 20, PointKind::Base), CurvePoint::new(64, 20, PointKind::Base), CurvePoint::new(20, 64, PointKind::Base), CurvePoint::new(20, 120, PointKind::Base), CurvePoint::new(20, 175, PointKind::Base), CurvePoint::new(64, 220, PointKind::Base), CurvePoint::new(120, 220, PointKind::Base), CurvePoint::new(175, 220, PointKind::Base), CurvePoint::new(220, 175, PointKind::Base), CurvePoint::new(220, 120, PointKind::Base), CurvePoint::new(220, 64, PointKind::Base), CurvePoint::new(175, 20, PointKind::Base), CurvePoint::new(120, 20, PointKind::Base), CurvePoint::new(120, 20, PointKind::Base), CurvePoint::new(160, 120, PointKind::Base), CurvePoint::new(130, 120, PointKind::Base), CurvePoint::new(130, 120, PointKind::Base), CurvePoint::new(130, 160, PointKind::Base), CurvePoint::new(110, 160, PointKind::Base), CurvePoint::new(110, 120, PointKind::Base), CurvePoint::new(80, 120, PointKind::Base), CurvePoint::new(120, 80, PointKind::Base), CurvePoint::new(160, 120, PointKind::Base)]
    );

}
