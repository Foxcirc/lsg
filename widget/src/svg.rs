
use std::mem;

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
        bytes::tag,
        character::{char, complete::{space0, digit1}},
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

    #[test]
    fn test_simple_path() {
        let (_, result) = path("M12 2C17.52 2 22 6.48 22 12C22 17.52 17.52 22 12 22C6.48 22 2 17.52 2 12C2 6.48 6.48 2 12 2ZM12 20C16.42 20 20 16.42 20 12C20 7.58 16.42 4 12 4C7.58 4 4 7.58 4 12C4 16.42 7.58 20 12 20ZM13 12V16H11V12H8L12 8L16 12H13Z")
            .expect("valid svg path");
        println!("{:?}", result)
    }

}

pub fn path_to_shape(path: Vec<PathCommand>) -> Vec<Point> {

    // step 1:
    // - split path into its connected sub-areas
    // - convert commands to list of points

    let mut areas = Vec::new();
    let mut current: Vec<CurvePoint> = Vec::new();
    let mut start = Point::default();
    let mut cursor = Point::default();

    for command in path {

        match command {
            PathCommand::Move(p1) => {
                // create a new sub area, if we already collected some points
                if current.len() > 0 {
                    let points = mem::take(&mut current);
                    areas.push(points);
                }
                // add the point
                current.push(CurvePoint::base((p1.x * 100.0) as i16, (p1.y * 100.0) as i16));
                start = p1;
                cursor = p1;
            },
            PathCommand::Line(p1) => {
                current.push(p1);
                cursor = p1;
            },
            PathCommand::Horizontal(pos) => {
                current.push(Point::new(pos, cursor.y));
                cursor.x += pos;
            },
            PathCommand::Vertical(pos) => {
                current.push(Point::new(cursor.x, pos));
                cursor.y += pos;
            },
            PathCommand::Quadratic(ctrl, pt) => {
                current.push(ctrl);
                current.push(pt);
                cursor = pt;
            },
            PathCommand::Return => {
                current.push(start);
                cursor = start;
            },
            _ => todo!("command not implemented yet"),
        }

    }

    todo!();

}
