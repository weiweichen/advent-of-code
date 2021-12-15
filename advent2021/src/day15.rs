pub mod day15 {

use std::cmp;
use std::collections;
use std::cmp::Ordering;

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let cave: Vec<Vec<i32>> = lines.iter().map(|line| {
        line.chars().collect::<Vec<char>>().iter().map(|c| {
            c.to_string().parse::<i32>().unwrap()
        }).collect()
    }).collect();

    let height = cave.len();
    let width = cave[0].len();

    // f(x, y) = min(f(x-1, y), f(x, y-1)) + cave(x, y)
    let mut result: Vec<Vec<i32>> = Vec::new();
    for y in 0..height  {
        let mut row: Vec<i32> = Vec::new();
        for x in 0..width {
            row.push(0);
        }
        result.push(row);
    }

    for y in 0..height  {
        for x in 0..width {
            if x == 0 && y == 0  {
                result[x][y] = 0; // cave[x][y];
            } else if y == 0  {
                result[x][y] = result[x - 1][y] + cave[x][y];
            } else if x == 0  {
                result[x][y] = result[x][y-1] + cave[x][y];
            } else  {
                result[x][y] = cmp::min(result[x - 1][y], result[x][y-1]) + cave[x][y];
            }
        }
    }

    result.iter().for_each( |row| {
       // println!("{:?}", row);
    });
    println!("shortest path: {}", result[width - 1][height - 1] );

}

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
    cost: i32,
    position: (usize, usize),
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl Ord for State {
    fn cmp(&self, other: &State) -> Ordering {
        // Notice that the we flip the ordering on costs.
        // In case of a tie we compare positions - this step is necessary
        // to make implementations of `PartialEq` and `Ord` consistent.
        other.cost.cmp(&self.cost)
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for State {
    fn partial_cmp(&self, other: &State) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn neighbours(r: usize, c: usize, num_rows: usize, num_cols: usize) -> Vec<(usize, usize)> {
    let mut result: Vec<(usize, usize)> = Vec::new();
    let dx_dy = vec![(-1, 0), (0, -1), (0, 1), (1, 0)];
    for i in 0..dx_dy.len() {
        let curr_r = r as i32 - dx_dy[i].0;
        let curr_c = c as i32 - dx_dy[i].1;
        if curr_r >= 0 && curr_r < num_rows as i32 && curr_c >= 0 && curr_c < num_cols as i32 {
            result.push((curr_r as usize, curr_c as usize));
        }
    }
    result
}


pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let cave: Vec<Vec<i32>> = lines.iter().map(|line| {
        line.chars().collect::<Vec<char>>().iter().map(|c| {
            c.to_string().parse::<i32>().unwrap()
        }).collect()
    }).collect();

    let height = cave.len();
    let width = cave[0].len();

    let mut workers: collections::BinaryHeap<State> = collections::BinaryHeap::new();
    let mut costs: collections::HashMap<(usize, usize), i32> = collections::HashMap::new();

    workers.push(State {cost: 0, position: (0, 0)});
    costs.entry((0, 0)).or_insert(0);
    // Examine the frontier with lower cost nodes first (min-heap)
    // Dijkstra's
    while let Some(State{ cost, position }) = workers.pop() {
        let x = position.0;
        let y = position.1;
        let points = neighbours(x, y, width * 5, height * 5);

        if x == width * 5 - 1 && y == height * 5 - 1  {
            println!("Found min cost {}", cost);
            break;
        }

        match costs.get(&(x, y))  {
            Some(c) =>  {if c < &cost  {continue;}},
            None => {},
        };

        points.iter().for_each(|(col, row)| {
            let tile_x = (*col / width) as i32;
            let tile_y = (*row / height) as i32;
            let xx = *col % width;
            let yy = *row % height;
            let curr_val = (cave[xx][yy] + tile_x + tile_y - 1) % 9 + 1;

            match costs.get(&(*col, *row))  {
                Some(c) => {
                    if *c > cost + curr_val {
                        costs.entry((*col, *row)).or_insert(cost + curr_val);
                        workers.push(State {cost: cost + curr_val, position: (*col, *row)});
                    };
                },
                None => {
                    costs.entry((*col, *row)).or_insert(cost + curr_val);
                    workers.push(State {cost: cost + curr_val, position: (*col, *row)});
                },
            };
        });
    }

}
}
