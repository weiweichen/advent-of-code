pub mod day25 {
use std::collections::HashMap;


fn step(sea: &mut Vec<Vec<char>>, height: usize, width: usize)->bool  {
    let mut change: bool = false;
    // move east
    for h in 0..height  {
        let mut x = 0;
        while x < width {
            // move from right to left
            let curr_x = x;
            let next_x = (curr_x + 1) % width;

            let mut to_move:bool = false;
            if sea[h][next_x] == 'X'  {
               sea[h][next_x] = '.';
            } else if sea[h][next_x] == '.' {
                to_move = true;
            }

            if sea[h][curr_x] != '>'  {
                x += 1;
                continue;
            }

            if to_move  {
                sea[h][next_x] = '>';
                if curr_x == 0 {
                    sea[h][curr_x] = 'X';
                } else  {
                    sea[h][curr_x] = '.';
                }
                change |= true;
                x += 1;
                let nn_x = (x+1)%width;
                if sea[h][nn_x] == 'X' {
                    sea[h][nn_x] = '.';
                }
            }
            x += 1;
        }
    }

    // print(sea);

    // move south
    for x in 0..width  {
        let mut h = 0;
        while h < height {
            // move from right to left
            let curr_h = h;
            let next_h = (curr_h + 1) % height;

            let mut to_move:bool = false;
            if sea[next_h][x] == 'X'  {
                sea[next_h][x] = '.';
            } else if sea[next_h][x] == '.' {
                to_move = true;
            }

            if sea[curr_h][x] != 'v'  {
                h += 1;
                continue;
            }

            if to_move {
                sea[next_h][x] = 'v';
                if curr_h == 0 {
                    sea[curr_h][x] = 'X';
                } else {
                    sea[curr_h][x] = '.';
                }
                change |= true;
                h += 1;

                let nn_h = (h+1)%height;
                if sea[nn_h][x] == 'X' {
                    sea[nn_h][x] = '.';
                }
            }
            h += 1;
        }
    }

    change
}

fn print(sea: &Vec<Vec<char>>)  {
    //println!("===================");
    for s in sea  {
        for p in s  {
            print!("{}", p);
        }
        println!("");
    }
}

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let mut sea_cucumbers: Vec<Vec<char>> = lines.iter().map(|line|  {
        line.chars().collect::<Vec<char>>()
    }).collect();

    let mut changed: bool = true;
    let height = sea_cucumbers.len();
    let width = sea_cucumbers[0].len();
    let mut steps: u32 = 0;

    while changed {
        changed = step(&mut sea_cucumbers, height, width);
        steps += 1;

        //println!("===================");
        //println!("After step {}", steps);
        //print(&sea_cucumbers);
    }
    println!("sea cucumbers stops at step {}", steps);

}


pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();


}

}
