pub mod day13 {

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let paper_coords: Vec<(i32, i32)> = lines.iter().filter(|line| !line.contains("fold along") && line.len() > 0).
        collect::<Vec<&String>>().iter().map( |line| {
            let s: Vec<&str> = line.split(",").collect();
        (s[0].parse::<i32>().unwrap(), s[1].parse::<i32>().unwrap())
    }).collect();

    println!("paper_coords.size() = {}", paper_coords.len());

    let fold_instructions: Vec<(char, i32)> =
        lines.iter().filter(|line| line.contains("fold along")).collect::<Vec<&String>>().
            iter().map(|line| {
                   let s: Vec<&str> = line.split("=").collect();
                   let s0: Vec<char> = s[0].chars().collect();
                   (s0[s0.len() - 1], s[1].parse::<i32>().unwrap())
                }
            ).collect();

    fold_instructions.iter().for_each(|(c, n)| {
        println!("fold along {} = {}", c, n);
    });

    let mut paper: std::collections::HashSet<(i32, i32)> = std::collections::HashSet::new();
    paper_coords.iter().for_each(|(x, y)| {
        paper.insert((*x, *y));
    });

    fold_instructions.iter().for_each(|(axis, num)| {
        let mut result_paper: std::collections::HashSet<(i32, i32)> = std::collections::HashSet::new();

        match (axis, num)  {
            ('x',  _) =>  {
                for (x, y) in paper.iter()  {
                    if x <= num  {
                        result_paper.insert((*x, *y));
                    } else  {
                        result_paper.insert(( 2 * num - *x, *y));
                    }
                }
                paper = result_paper;
                println!("after fold along {} = {}, result: {}", axis, num, paper.len());
            },
            ('y', _) =>  {
                for (x, y) in paper.iter()  {
                    if y <= num  {
                        result_paper.insert((*x, *y));
                    } else  {
                        result_paper.insert(( *x, 2* num - *y));
                    }
                }
                paper = result_paper;
                println!("after fold along {} = {}, result: {}", axis, num, paper.len());
            },
            _ => println!(""),
        };
    });
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let paper_coords: Vec<(i32, i32)> = lines.iter().filter(|line| !line.contains("fold along") && line.len() > 0).
        collect::<Vec<&String>>().iter().map( |line| {
        let s: Vec<&str> = line.split(",").collect();
        (s[0].parse::<i32>().unwrap(), s[1].parse::<i32>().unwrap())
    }).collect();

    println!("paper_coords.size() = {}", paper_coords.len());

    let fold_instructions: Vec<(char, i32)> =
        lines.iter().filter(|line| line.contains("fold along")).collect::<Vec<&String>>().
            iter().map(|line| {
            let s: Vec<&str> = line.split("=").collect();
            let s0: Vec<char> = s[0].chars().collect();
            (s0[s0.len() - 1], s[1].parse::<i32>().unwrap())
        }
        ).collect();

    fold_instructions.iter().for_each(|(c, n)| {
        println!("fold along {} = {}", c, n);
    });

    let mut paper: std::collections::HashSet<(i32, i32)> = std::collections::HashSet::new();
    paper_coords.iter().for_each(|(x, y)| {
        paper.insert((*x, *y));
    });

    fold_instructions.iter().for_each(|(axis, num)| {
        let mut result_paper: std::collections::HashSet<(i32, i32)> = std::collections::HashSet::new();

        match (axis, num)  {
            ('x',  _) =>  {
                for (x, y) in paper.iter()  {
                    if x <= num  {
                        result_paper.insert((*x, *y));
                    } else  {
                        result_paper.insert(( 2 * num - *x, *y));
                    }
                }
                paper = result_paper;
                println!("after fold along {} = {}, result: {}", axis, num, paper.len());
            },
            ('y', _) =>  {
                for (x, y) in paper.iter()  {
                    if y <= num  {
                        result_paper.insert((*x, *y));
                    } else  {
                        result_paper.insert(( *x, 2* num - *y));
                    }
                }
                paper = result_paper;
                println!("after fold along {} = {}, result: {}", axis, num, paper.len());
            },
            _ => println!(""),
        };
    });

    let mut max_x: i32 = 0;
    let mut max_y: i32 = 0;
    paper.iter().for_each(|&(x, y)| {
        if x > max_x  {
            max_x = x;
        }
        if y > max_y  {
            max_y = y;
        }
    });

        for y in 0..=max_y  {
            for x in 0..=max_x  {
            if paper.contains(&(x, y)) {
                print!("##");
            } else  {
                print!("..");
            }
        }
        println!("");
    }
}
}
