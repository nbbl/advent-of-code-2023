use std::collections::HashMap;

use aoc_parse::{parser, prelude::*};
use thiserror::Error;

const INPUT_DATA: &str = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4";

const FULL_INPUT: &str = include_str!("data/input_day_4.txt");
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct ConversionRange {
    src: u64,
    dst: u64,
    len: u64,
}

impl ConversionRange {
    fn convert(self: &Self, n: &u64) -> Option<u64> {
        match n {
            &n if (n >= self.src && n < (self.src + self.len)) => Some(self.dst + n - self.src),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct ConversionMap {
    from: String,
    to: String,
    ranges: Vec<ConversionRange>,
}

impl ConversionMap {
    fn convert(self: &Self, n: &u64) -> u64 {
        self.ranges
            .iter()
            .find_map(|range| range.convert(n))
            .unwrap_or(n.clone())
    }
    fn convert_range(self: &Self, input_range: &ConversionRange) -> Vec<ConversionRange> {
        let start = input_range.dst;
        let stop = input_range.dst + input_range.len;
        let mut matches: Vec<_> = self.ranges.iter().filter(|r| stop > r.src && start < r.src + r.len).collect::<Vec<_>>();
        matches.sort_by_key(|r| r.src);
        let mut result: Vec<ConversionRange> = Vec::new();
        let mut current = input_range.dst;
        for r in matches {
            if r.src > current { // add a filler conversion range
                result.push(ConversionRange { src: input_range.src + (current - start), dst: input_range.dst + (current - start), len: r.src - current });
                current = r.src;
            }
            let new_start = current.max(r.src);
            let new_stop = stop.min(r.src + r.len);
            let new_len = new_stop - new_start;
            result.push(ConversionRange { src: input_range.src + (current - start), dst: r.dst + (new_start - r.src), len: new_len });
            current = new_stop;
        }
        if current < stop {
            result.push(ConversionRange { src: input_range.src + (current - start), dst: input_range.dst + (current - start), len: stop - current })
        }
        result
    }
}

fn main() -> Result<()> {
    run()
}
fn parse_seeds_1(input: &str) -> Result<Vec<u64>> {
    Ok(parser!("seeds:" (" " u64)+ "\n\n").parse(input)?)
}

fn parse_seeds_2(input: &str) -> Result<Vec<ConversionRange>> {
    Ok(parser!(
            "seeds: "
            repeat_sep(src:u64 " " len:u64 => ConversionRange { src, dst: src, len}, " ")
            "\n\n"
    )
    .parse(input)?)
}

fn parse_maps(input: &str) -> Result<Vec<ConversionMap>> {
    Ok(parser!(
            sections(
                from:string(alnum+) "-to-" to:string(alnum+) " map:\n"
                ranges:lines(dst:u64 " " src:u64 " " len:u64 => ConversionRange { dst, src, len })
                => ConversionMap {from, to, ranges}
            )
        )
        .parse(input)?)
}

fn run() -> Result<()> {
    let input = FULL_INPUT;
    println!("{:?}", input);
    let (seeds_str, input) = input.split_at(input.find("\n\n").unwrap() + 2);
    dbg!(seeds_str);
    let seeds_1: Vec<u64> = parse_seeds_1(seeds_str)?;
    dbg!(&seeds_1);
    let seeds_2: Vec<ConversionRange> = parse_seeds_2(seeds_str)?;
    dbg!(&seeds_2);
    let steps: Vec<ConversionMap> = parse_maps(input)?;

    let mut full_map: HashMap<String, Vec<u64>> = HashMap::new();
    full_map.insert("seed".into(), seeds_1);
    for map in &steps {
        dbg!(&map.from);
        dbg!(&map.to);
        let new_values = full_map
            .get(&map.from)
            .expect("Should be computed before")
            .iter()
            .map(|seed| map.convert(seed))
            .collect::<Vec<u64>>();
        full_map.insert(map.to.clone(), new_values);
    }

    let mut base = seeds_2;
    for map in &steps {
        base = base.iter().flat_map(|range| map.convert_range(range)).collect();
        base.sort_by_key(|range| range.src);
    }
    dbg!(full_map.get("location").unwrap().iter().min().unwrap());
    dbg!(&base);
    dbg!(base.iter().min_by_key(|range| range.dst).unwrap().dst);
    Ok(())
}
