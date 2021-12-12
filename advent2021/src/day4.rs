pub mod day4 {


fn mark_board(boards: &Vec<Vec<Vec<i32>>>, mark: &mut Vec<Vec<Vec<bool>>>, value:i32 )  {
   for b in 0..boards.len()  {
       for i in 0..5 {
           for j in 0..5 {
                if boards[b][i][j] == value  {
                    mark[b][i][j] = true;
                }
           }
       }
   }
}

fn check_bingo(mark: &Vec<Vec<Vec<bool>>>) -> i32{
    for i in 0..mark.len()  {
        for r in 0..5 {
            let mut found: bool = true;
            for c in 0..5 {
                found &= mark[i][r][c];
            }
            if found  {
                return i as i32;
            }
        }
        for r in 0..5 {
            let mut found: bool = true;
            for c in 0..5 {
                found &= mark[i][c][r];
            }
            if found  {
                return i as i32;
            }
        }
    }
    -1
}

fn mark_bingo(mark: &Vec<Vec<Vec<bool>>>, timestamp:&mut Vec<i32>, step: i32) {
    for i in 0..mark.len()  {
        if timestamp[i] > 0  {
            continue;
        }

        for r in 0..5 {
            let mut found: bool = true;
            for c in 0..5 {
                found &= mark[i][r][c];
            }
            if found  {
                timestamp[i] = step;
                break;
            }
        }
        if timestamp[i] > 0  {
            continue;
        }

        for r in 0..5 {
            let mut found: bool = true;
            for c in 0..5 {
                found &= mark[i][c][r];
            }
            if found  {
                timestamp[i] = step;
                break;
            }
        }
    }
}

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let numbers: Vec<i32> = lines[0].split(",").map(|n| n.parse::<i32>().unwrap()).collect();
    let boards: Vec<Vec<Vec<i32>>> =
        lines[2..lines.len()].chunks(6).map(|chunk| chunk[0..5].iter().map(|line| line.split_whitespace().map(|n| n.parse::<i32>().unwrap()).collect()).collect()).collect();

    let mut mark: Vec<Vec<Vec<bool>>> = Vec::with_capacity(boards.len() * 25);
    for _ in 0..boards.len()  {
        let mut m: Vec<Vec<bool>> = Vec::with_capacity(25);
        for _ in 0..5 {
            m.push(vec![false;5]);
        }
        mark.push(m);
    }

    let mut result_board: i32 = -1;
    let mut curr_n: usize = 0;

    while result_board == -1 && curr_n < numbers.len()  {
        mark_board(&boards, &mut mark, numbers[curr_n]);
        curr_n += 1;
        result_board = check_bingo(&mark);
    }


    if result_board >= 0 {
        let f_board : Vec<i32> = boards[result_board as usize].clone().into_iter().flatten().collect();
        let f_mark : Vec<bool> = mark[result_board as usize].clone().into_iter().flatten().collect();
        let unmarked_value = f_board.iter().zip(f_mark.iter()).filter(|(_, m)| **m == false).map(|(v, _)| v).sum::<i32>();
        println!("bingo board {} after, {} steps, unmarked_value_sum {}, won number {}, r {}",
                 result_board, curr_n, unmarked_value, numbers[curr_n - 1], unmarked_value * numbers[curr_n - 1]);
    }

}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let numbers: Vec<i32> = lines[0].split(",").map(|n| n.parse::<i32>().unwrap()).collect();
    let boards: Vec<Vec<Vec<i32>>> =
        lines[2..lines.len()].chunks(6).map(|chunk| chunk[0..5].iter().map(|line| line.split_whitespace().map(|n| n.parse::<i32>().unwrap()).collect()).collect()).collect();

    let mut mark: Vec<Vec<Vec<bool>>> = Vec::with_capacity(boards.len() * 25);
    let mut timestamp: Vec<i32> = vec![0;boards.len()];

    for _ in 0..boards.len()  {
        let mut m: Vec<Vec<bool>> = Vec::with_capacity(25);
        for _ in 0..5 {
            m.push(vec![false;5]);
        }
        mark.push(m);
    }

    let mut curr_n: usize = 0;
    let mut marked_board: i32 = 0;

    while marked_board < (boards.len() as i32) && curr_n < numbers.len()  {
        mark_board(&boards, &mut mark, numbers[curr_n]);
        curr_n += 1;
        mark_bingo(&mark, &mut timestamp, curr_n as i32);

        marked_board = timestamp.iter().filter(|t| **t > 0).collect::<Vec<&i32>>().len() as i32;
    }

    //let t_result = timestamp.iter().enumerate().fold(0, |t1, t2| *t1.1.max(t2.1));
    let t_result = timestamp.iter().enumerate().fold((0, 0), |(i1, t1), (i2, &t2)|
        if t1 > t2 {(i1, t1) } else {(i2, t2)}
    );
    let result_board = t_result.0 as i32;

    for t in timestamp.iter().enumerate()  {
        println!(" {}  {} {}", t.0, t.1, result_board);
    }

    if result_board >= 0 {
        let f_board : Vec<i32> = boards[result_board as usize].clone().into_iter().flatten().collect();
        let f_mark : Vec<bool> = mark[result_board as usize].clone().into_iter().flatten().collect();
        let unmarked_value = f_board.iter().zip(f_mark.iter()).filter(|(_, m)| **m == false).map(|(v, _)| v).sum::<i32>();
        println!("bingo board {} after, {} steps, unmarked_value_sum {}, won number {}, r {}",
                 result_board, curr_n, unmarked_value, numbers[curr_n - 1], unmarked_value * numbers[curr_n - 1]);
    }

}

}
