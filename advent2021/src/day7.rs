pub mod day7 {

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let inputs: Vec<u64> = lines[0].split(",").map(|n| n.parse::<u64>().unwrap()).collect();
    let (min, max) = inputs.iter().fold((inputs[0], inputs[0]), |(curr_min, curr_max), &n| if n < curr_min {(n, curr_max)}
    else if n > curr_max {(curr_min, n)} else {(curr_min, curr_max)});

    let mut sum = max * inputs.len() as u64;
    let result = (min..=max).fold(sum, |s, i| {
        let curr_sum = inputs.iter().fold(0, |sum, &n| {
            let abs = if i > n { i - n } else { n - i };
            sum + abs
        });
        if curr_sum < s {curr_sum} else {s}
    });

    println!("number inputs l:{} result:{}", inputs.len(), result);
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let inputs: Vec<u64> = lines[0].split(",").map(|n| n.parse::<u64>().unwrap()).collect();
    let (min, max) = inputs.iter().fold((inputs[0], inputs[0]), |(curr_min, curr_max), &n| if n < curr_min {(n, curr_max)}
    else if n > curr_max {(curr_min, n)} else {(curr_min, curr_max)});

    let mut sum = ((max - min) * (max - min + 1) / 2) * (inputs.len() as u64);
    let result = (min..=max).fold(sum, |s, i| {
        let curr_sum = inputs.iter().fold(0, |sum, &n| {
            let abs = if i > n { i - n } else { n - i };
            sum + abs * (abs + 1) / 2
        });
        if curr_sum < s {curr_sum} else {s}
    });
    println!("number inputs l:{} result:{}", inputs.len(), result);
}
}
