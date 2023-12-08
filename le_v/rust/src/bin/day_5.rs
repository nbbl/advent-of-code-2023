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
    println!("Day 5: {:?}", day_5::solve()?);
    Ok(())
}

mod day_5 {
    use super::*;
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
    const FULL_INPUT: &str = include_str!("../data/input_day_5.txt");

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
    struct ConversionRange {
        src: u64,
        dst: u64,
        len: u64,
    }
    impl ConversionRange {
        fn convert(&self, n: &u64) -> Option<u64> {
            match n {
                &n if (n >= self.src && n < self.src_stop()) => Some(self.dst + n - self.src),
                _ => None,
            }
        }
        fn src_stop(&self) -> u64 {
            self.src + self.len
        }
        fn dst_stop(&self) -> u64 {
            self.dst + self.len
        }
    }

    #[derive(Debug)]
    struct ConversionMap {
        ranges: Vec<ConversionRange>,
    }

    impl ConversionMap {
        fn convert(&self, n: &u64) -> u64 {
            self.ranges
                .iter()
                .find_map(|range| range.convert(n))
                .unwrap_or(*n)
        }
        fn convert_range(&self, input_range: &ConversionRange) -> Vec<ConversionRange> {
            // input is a conversion from X to Y, self converts from Y to Z, this returns a conversion from X to Z.
            let start_y = input_range.dst;
            let stop_y = input_range.dst_stop();

            // Get ranges from self whose source (in Y space) intersect the target (in Y space) of input_range.
            let mut relevant_y_to_z: Vec<_> = self
                .ranges
                .iter()
                .filter(|y_to_z| stop_y > y_to_z.src && start_y < y_to_z.src_stop())
                .collect();
            relevant_y_to_z.sort_by_key(|r| r.src);
            let relevant_y_to_z = relevant_y_to_z;

            let mut x_to_z: Vec<ConversionRange> = Vec::new();
            let mut current_y = start_y;
            for y_to_z in relevant_y_to_z {
                if y_to_z.src > current_y {
                    // intersection starts further than current_y
                    // We keep original values.
                    x_to_z.push(ConversionRange {
                        src: input_range.src + (current_y - start_y),
                        dst: input_range.dst + (current_y - start_y),
                        len: y_to_z.src - current_y,
                    });
                    current_y = y_to_z.src;
                }
                // intersection starts now
                let new_start_y = current_y;
                let new_stop_y = stop_y.min(y_to_z.src_stop()); // don't go further than needed
                let new_len = new_stop_y - new_start_y;
                x_to_z.push(ConversionRange {
                    src: input_range.src + (current_y - start_y),
                    dst: y_to_z.dst + (new_start_y - y_to_z.src),
                    len: new_len,
                });
                current_y = new_stop_y;
            }
            // no intersection left in [current_y, stop_y[ so we keep original values
            if current_y < stop_y {
                x_to_z.push(ConversionRange {
                    src: input_range.src + (current_y - start_y),
                    dst: input_range.dst + (current_y - start_y),
                    len: stop_y - current_y,
                })
            }
            x_to_z
        }
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
                    string(alnum+) "-to-" string(alnum+) " map:\n"
                    ranges:lines(dst:u64 " " src:u64 " " len:u64 => ConversionRange { dst, src, len })
                    => ConversionMap {ranges}
                )
            )
        .parse(input)?)
    }

    pub fn solve() -> Result<(u64, u64)> {
        let input = FULL_INPUT;

        let (seeds_str, input) = input.split_at(input.find("\n\n").unwrap() + 2);
        let seeds_1: Vec<u64> = parse_seeds_1(seeds_str)?;
        let seeds_2: Vec<ConversionRange> = parse_seeds_2(seeds_str)?;
        let steps: Vec<ConversionMap> = parse_maps(input)?;

        let locations = steps.iter().fold(seeds_1, |values, step| {
            values.iter().map(|v| step.convert(v)).collect()
        });

        let transforms = steps.iter().fold(seeds_2, |transforms, step| {
            transforms
                .iter()
                .flat_map(|range| step.convert_range(range))
                .collect()
        });
        Ok((
            *locations.iter().min().unwrap(),
            transforms.iter().min_by_key(|range| range.dst).unwrap().dst,
        ))
    }
}
