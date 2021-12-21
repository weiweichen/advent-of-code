pub mod day21 {

use cached::proc_macro::cached;
pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let start1: u32 = lines[0].split(": ").collect::<Vec<&str>>()[1].parse::<u32>().unwrap();
    let start2: u32 = lines[1].split(": ").collect::<Vec<&str>>()[1].parse::<u32>().unwrap();
    println!("player1 starts at: {}", start1);
    println!("player2 starts at: {}", start2);

    let mut curr_player:usize = 0;
    let mut scores: Vec<u32> = vec![0, 0];
    let mut positions: Vec<u32> = vec![start1, start2];
    let mut dice:u32 = 0;
    let dice_faces = 100;
    let mut times:u64 = 0;

    loop {
        let first_roll = (dice) % dice_faces + 1;
        let second_roll = (dice + 1) % dice_faces + 1;
        let third_roll = (dice + 2) % dice_faces + 1;

        let next_pos = (positions[curr_player] + first_roll + second_roll + third_roll - 1) % 10 + 1;
        positions[curr_player] = next_pos;
        scores[curr_player] += next_pos;
        println!("Player {}, rolls {} +  {} +  {} and moves to space {} for a total score of {}", curr_player + 1, first_roll, second_roll, third_roll, next_pos, scores[curr_player]);
        times += 3;
        if scores[curr_player] >= 1000 {
            break;
        }
        dice = third_roll;
        curr_player = (curr_player + 1)% 2;
    }
    let lose_player = (curr_player + 1)%2;

    println!("Dice had been rolled {} times, the losing player {} had {} points, result {}"
             , times, lose_player + 1, scores[lose_player], times * scores[lose_player] as u64);

}

#[cached]
fn cache_calc_universes(curr_pos: u32, other_pos: u32, curr_score: u32, other_score: u32) ->[u64;2] {
    let mut results = [0;2];

    let mut scores :Vec<u32> = vec![3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9];

    for s in scores  {
        let new_pos = (curr_pos + s - 1) % 10 + 1;
        let new_score = curr_score + new_pos;
        if new_score >= 21  {
            results[0] += 1;
        } else  {
            let sub_results = cache_calc_universes(other_pos, new_pos, other_score, new_score);
            results[1] += sub_results[0];
            results[0] += sub_results[1];
        }
    }
    results
}

fn dp_calc_universes(curr_pos: &u32, other_pos: &u32, curr_score: &u32, other_score: &u32,
                     paths: &u64, active_player: &u32, wins: &mut[u64;2], probs: &Vec<u64>) {
    for i in 3..10 {
        if *active_player == 0 {
            let new_pos = (curr_pos + i - 1) % 10 + 1;
            let new_score = curr_score + new_pos;
            let new_paths = paths * probs[i as usize];
            if new_score >= 21 {
                wins[0] += new_paths;
            } else {
                dp_calc_universes(&new_pos, other_pos, &new_score, other_score, &new_paths, &1, wins, probs);
            }
        } else  {
            let new_pos = (other_pos + i - 1) % 10 + 1;
            let new_score = other_score + new_pos;
            let new_paths = paths * probs[i as usize];
            if new_score >= 21 {
                wins[1] += new_paths;
            } else {
                dp_calc_universes(curr_pos, &new_pos, curr_score, &new_score, &new_paths, &0, wins, probs);
            }
        }
    }
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let start1: u32 = lines[0].split(": ").collect::<Vec<&str>>()[1].parse::<u32>().unwrap();
    let start2: u32 = lines[1].split(": ").collect::<Vec<&str>>()[1].parse::<u32>().unwrap();
    println!("player1 starts at: {}", start1);
    println!("player2 starts at: {}", start2);

    let scores :Vec<u32> = vec![3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9];
    let probs: Vec<u64> = vec![0, 0, 0, 1, 3, 6, 7, 6, 3, 1];
    let mut wins = [0;2];

    dp_calc_universes(&start1, &start2, &0, &0, &1, &0, &mut wins, &probs);
    println!("results {:?}, max {}", wins, wins.iter().max().unwrap());

    // cheating from reddit solution, use cached for caching
    //let result = cache_calc_universes(start1, start2, 0, 0);
    //println!("results {:?}, mac {}", result, result.iter().max().unwrap());
}

}
