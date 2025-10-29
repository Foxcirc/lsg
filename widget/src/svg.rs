
use std::{f64::consts::PI, iter::{once, Rev}, mem};

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
    points: Vec<CurvePoint>,
    area: usize,
    direction: SectionDirection,
    fill: bool,
}

pub fn scale_all_points(shape: &mut [CurvePoint], factor: f32) {
    for point in shape {
        point.x = (point.x as f32 * factor) as i16;
        point.y = (point.y as f32 * factor) as i16;
    }
}

pub fn path_to_shape(path: Vec<PathCommand>) -> Vec<CurvePoint> {

    // Step 1:
    // Convert commands to list of points and split the path into sub-sections that need to be
    // connected by an invisible line. Holes or filled regions inside holes are considered sub-sections.

    let mut sections: Vec<Section> = Vec::new();
    let mut current: Vec<CurvePoint> = Vec::new();
    let mut start = Point::default();
    let mut cursor = Point::default();

    for command in path {

        match command {
            PathCommand::Move(p1) => {
                // create a new sub area, if we already collected some points
                if current.len() > 0 {
                    let mut points = mem::take(&mut current);
                    points.dedup(); // TODO: avoid these dedups
                    sections.push(Section {
                        points,
                        ..Default::default()
                    });
                }
                // add the point
                current.push(CurvePoint::base_from_point(p1 * 100.0));
                start = p1;
                cursor = p1;
            },
            PathCommand::Line(p1) => {
                current.push(CurvePoint::base_from_point(p1 * 100.0));
                cursor = p1;
            },
            PathCommand::Horizontal(pos) => {
                current.push(CurvePoint::base_from_point(Point::new(pos, cursor.y) * 100.0));
                cursor.x = pos;
            },
            PathCommand::Vertical(pos) => {
                current.push(CurvePoint::base_from_point(Point::new(cursor.x, pos) * 100.0));
                cursor.y = pos;
            },
            PathCommand::Quadratic(ct, p1) => {
                current.push(CurvePoint::ctrl_from_point(ct * 100.0));
                current.push(CurvePoint::base_from_point(p1 * 100.0));
                cursor = p1;
            },
            PathCommand::Cubic(c1, c2, p1) => {
                // current.push(CurvePoint::ctrl_from_point(c1 * 100.0));
                current.push(CurvePoint::base_from_point(c2 * 100.0));
                current.push(CurvePoint::base_from_point(p1 * 100.0));
                cursor = p1;
            },
            PathCommand::Return => {
                current.push(CurvePoint::base_from_point(start * 100.0));
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

        // the number of sections the `oidx` section is contained inside of
        let mut parents = 0;

        for inner_idx in 0..sections.len() {

            if outer_idx == inner_idx { continue };

            // we only need to test one point, since we assume no self-intersections
            let test = sections[outer_idx].points[0];

            // the total winding number
            let mut winding = 0.0;

            let points = &sections[inner_idx].points;
            for idx in 0..points.len() {
                let p1 = points[idx];
                let p2 = points[(idx + 1) % points.len()];
                let angle1 = ((p1.y() - test.y()) as f64).atan2((p1.x() - test.x()) as f64);
                let angle2 = ((p2.y() - test.y()) as f64).atan2((p2.x() - test.x()) as f64);
                let mut diff = angle2 - angle1;
                if      diff >  PI { diff -= 2.0 * PI; }
                else if diff < -PI { diff += 2.0 * PI; }
                winding += diff;
            }

            if winding.abs() > 1e-5 {
                parents += 1;
            }

        }

        // fill using even-odd rule, for now
        sections[outer_idx].fill = parents % 2 == 0;

    }

    // Step 4:
    // Connect all the sections into one shape by drawing invisible lines between them.

    // TODO: Could this step generate any meaningfull information for ear clipping later on? Would be nice to save some costs.

    // println!("there are {} sections in total", sections.len());
    // println!("{sections:#?}");

    let mut result: Vec<CurvePoint> = Vec::new();
    let mut removed: Vec<bool> = Vec::new();
    removed.resize(sections.len(), false);

    // Push the first section. All other sections will be connected
    // to this section one after another.
    if sections.len() > 0 {
        let section = sections.remove(0); // TODO: benchmark with swap_remove vs. remove
        let reverse = section.fill && section.direction == SectionDirection::Cw ||
                      !section.fill && section.direction == SectionDirection::Ccw;
        if !reverse { result.extend_from_slice(&section.points) }
        else        { result.extend(section.points.iter().rev()) }
    }

    // We try to make a connection between any point in the already
    // generated result and a point from another section.
    for (section_idx, section) in sections.iter().enumerate() {

        if removed[section_idx] {
            continue;
        }

        if let Some(spot) = find_connection_spot(&result, &section.points, &sections) {

            let mut new = Vec::new();

            for point in result.iter().skip(spot.lhs).chain(result.iter().take(spot.lhs + 1)) {
                new.push(*point);
            }

            let reverse = section.fill && section.direction == SectionDirection::Cw ||
                          !section.fill && section.direction == SectionDirection::Ccw;
            let idx = if !reverse { spot.rhs } else { section.points.len() - 1 - spot.rhs };
            let iter = PossiblyReversed::new(section.points.iter(), reverse);
            for point in iter.clone().skip(idx).chain(iter.take(idx + 1)) {
                new.push(*point);
            }
            // TODO: mark section as removed
            removed[section_idx] = true;

            return  new

        }

    }

    todo!()

}

/// Finds indices in `lhs` and `rhs` where a new edge could be created without intersecting any
/// of the edges present in `sections`.
fn find_connection_spot(lhs: &[CurvePoint], rhs: &[CurvePoint], sections: &[Section]) -> Option<ConnectionSpot> {

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

            for check_section in sections.iter() {
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

#[derive(Clone)]
enum PossiblyReversed<I: DoubleEndedIterator> {
    Normal(I),
    Rev(Rev<I>),
}

impl<I: DoubleEndedIterator> PossiblyReversed<I> {
    pub fn new<T: IntoIterator<IntoIter = I>>(input: T, reverse: bool) -> Self {
        let iter = input.into_iter();
        if !reverse { Self::normal(iter) } else { Self::rev(iter) }
    }
    pub fn normal(iter: I) -> Self {
        Self::Normal(iter)
    }
    pub fn rev(iter: I) -> Self {
        Self::Rev(iter.rev())
    }
}

impl<I: DoubleEndedIterator> Iterator for PossiblyReversed<I> {
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Normal(iter) => iter.next(),
            Self::Rev(iter) => iter.next()
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct ConnectionSpot {
    pub lhs: usize,
    pub rhs: usize,
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
    let shape = path_to_shape(points);

    assert_eq!(&shape, &[
        CurvePoint::base(120, 20),
        CurvePoint::base(64, 20),
        CurvePoint::base(20, 120),
        CurvePoint::base(20, 175),
        CurvePoint::base(120, 220),
        CurvePoint::base(175, 220),
        CurvePoint::base(220, 120),
        CurvePoint::base(220, 64),
        CurvePoint::base(120, 20),
        CurvePoint::base(160, 120),
        CurvePoint::base(130, 120),
        CurvePoint::base(130, 160),
        CurvePoint::base(110, 160),
        CurvePoint::base(110, 120),
        CurvePoint::base(80, 120),
        CurvePoint::base(120, 80),
        CurvePoint::base(160, 120),
    ]);

}
