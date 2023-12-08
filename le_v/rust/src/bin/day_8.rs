use std::collections::HashMap;

use aoc_parse::{parser, prelude::*};
use thiserror::Error;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("unknown error: {0}")]
    Unknown(String),
    #[error(transparent)]
    IOError {
        #[from]
        source: std::io::Error,
    },
    #[error(transparent)]
    ParseError {
        #[from]
        source: aoc_parse::ParseError,
    },
}

fn main() -> Result<()> {
    const SAMPLE: &str = include_str!("../data/input_day_8.txt");
    let (directions, nodes) = parse_input(SAMPLE)?;
    let path_human = traverse_count(&directions, &nodes, |&x| x == "AAA", |&y| y == "ZZZ");
    println!("Length of path for a human: {}", path_human);
    let path_ghost = traverse_count(
        &directions,
        &nodes,
        |&x| x.ends_with("A"),
        |&y| y.ends_with("Z"),
    );
    println!("Length of path for a ghost: {}", path_ghost);
    Ok(())
}

#[derive(Debug)]
enum Direction {
    RIGHT,
    LEFT,
}

fn parse_input(input_str: &str) -> Result<(Vec<Direction>, HashMap<String, (String, String)>)> {
    let (directions, nodes) = parser!(
        directions:({"L" => Direction::LEFT, "R" => Direction::RIGHT})+ "\n\n"
        section(
            nodes:lines(
                node:string(alnum+) " = (" left:string(alnum+) ", " right:string(alnum+) ")" => (node, (left, right))
            ) => nodes
        )
    ).parse(input_str)?;
    let nodes: HashMap<String, (String, String)> = nodes.into_iter().collect();
    Ok((directions, nodes))
}

fn traverse_count<'a, F, G>(
    directions: &[Direction],
    nodes: &'a HashMap<String, (String, String)>,
    start: F,
    finish: G,
) -> u64
where
    F: Fn(&&String) -> bool,
    G: Fn(&&String) -> bool,
{
    let mut positions: Vec<&'a String> = nodes.keys().into_iter().filter(start).collect();
    let mut current: u64 = 1;

    for (path_len, direction) in (1u64..).zip(directions.iter().cycle()) {
        let previous_len = positions.len();
        positions = positions
            .iter()
            .map(|&position| match direction {
                Direction::LEFT => &nodes.get(position).unwrap().0,
                Direction::RIGHT => &nodes.get(position).unwrap().1,
            })
            .filter(|x| !finish(x))
            .collect();
        let new_len = positions.len();
        if new_len == previous_len {
            continue;
        }
        current = num::integer::lcm(current, dbg!(path_len));
        dbg!(&current);
        if new_len == 0 {
            break;
        }
    }
    current
}

mod tests {
    use super::*;

    const EXAMPLE_DATA_1: &str = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
";
    const EXAMPLE_DATA_2: &str = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)";

    #[test]
    fn test_part1() -> Result<()> {
        let (directions, nodes) = parse_input(EXAMPLE_DATA_1)?;
        let path = traverse_count(&directions, &nodes, |&x| x == "AAA", |&y| y == "ZZZ");
        dbg!(&path);
        assert_eq!(path, 2);

        let (directions, nodes) = parse_input(EXAMPLE_DATA_2)?;
        let path = traverse_count(&directions, &nodes, |&x| x == "AAA", |&y| y == "ZZZ");
        dbg!(&path);
        assert_eq!(path, 6);
        Ok(())
    }

    const EXAMPLE_DATA_3: &str = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)";

    #[test]
    fn test_part2() -> Result<()> {
        let (directions, nodes) = parse_input(EXAMPLE_DATA_3)?;
        let path = traverse_count(
            &directions,
            &nodes,
            |&x| x.ends_with("A"),
            |&y| y.ends_with("Z"),
        );
        dbg!(&path);
        assert_eq!(path, 6);
        Ok(())
    }
}
