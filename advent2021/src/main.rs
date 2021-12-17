
use std::fs::File;
use std::io::BufReader;

mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;

fn main() {
    let file = File::open("tests/day17.txt").expect("file wasn't found.");
    let mut reader = BufReader::new(file);
    day17::day17::part1(&mut reader);
}