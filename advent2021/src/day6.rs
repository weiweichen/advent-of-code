pub mod day6 {

fn run(inputs :Vec<usize>, day: i32) -> u64 {
    let mut state: Vec<u64> = vec![0; 9];
    inputs.iter().for_each(|d| state[*d] += 1);
    for day in 0..day {
        state = state.iter().enumerate().map(|(i, _)| if i == 6 {
            state[i + 1] + state[0]
        } else if i == 8 { state[0] } else { state[i + 1] }).collect();
    }

    state.iter().fold(0, |sum, d| sum + *d)
}

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let inputs: Vec<usize> = lines[0].split(",").map(|n| n.parse::<usize>().unwrap()).collect();
    println!("number inputs l:{} result:{}", inputs.len(), run(inputs, 80));
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let inputs: Vec<usize> = lines[0].split(",").map(|n| n.parse::<usize>().unwrap()).collect();
    println!("number inputs l:{} result:{}", inputs.len(), run(inputs, 256));

}
}
