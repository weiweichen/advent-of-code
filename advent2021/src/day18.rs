pub mod day18 {

fn add(input1: &Vec<String>, input2: &Vec<String>) -> Vec<String>  {
    let mut result: Vec<String> = Vec::new();
    let mut brackets: Vec<&str> = Vec::new();
    let mut processed: Vec<String> = Vec::new();

    result.push("[".to_string());
    result.extend(input1.clone());
    result.push(",".to_string());
    result.extend(input2.clone());
    result.push("]".to_string());

    //result.iter().for_each(|s| print!("{}", s));
    //println!("");


    let mut reduced: bool = true;
    let mut prev_run_explode: bool = true;
    let mut can_split: bool = false;
    let mut first_run: bool = true;

    while reduced || (can_split || first_run) {
        first_run = false;
        let mut prev_number_pos: i32 = -1;
        let mut i: usize = 0;
        let mut curr_run_explode : bool = false;
        reduced = false;

        brackets.clear();
        processed.clear();
        while i < result.len() {
            if result[i] == "[" {
                if brackets.len() < 4 {
                    brackets.push("[");
                    processed.push(result[i].clone());
                } else {
                    // explode
                    let left_value = result[i + 1].to_string().parse::<u8>().unwrap();
                    let right_value = result[i + 3].to_string().parse::<u8>().unwrap();
                    //println!("explode [{}, {}]", left_value, right_value);
                    // update left
                    if prev_number_pos >= 0 {
                        let prev_value = processed[prev_number_pos as usize].to_string().parse::<u8>().unwrap();
                        processed[prev_number_pos as usize] = (prev_value + left_value).to_string();
                    }
                    // update right
                    processed.push("0".to_string());

                    let mut added: bool = false;
                    curr_run_explode = true;

                    for j in i + 5..result.len() {
                        if result[j] != "[" && result[j] != "," && result[j] != "]" && !added {
                            let next_value = result[j].to_string().parse::<u8>().unwrap();
                            processed.push((next_value + right_value).to_string());
                            added = true;
                        } else {
                            processed.push(result[j].clone());
                        }
                    }
                    reduced = true;
                    break;
                }
            } else if result[i] == "]" {
                processed.push(result[i].clone());
                brackets.pop();
            } else if result[i] == "," {
                processed.push(result[i].clone());
            } else {
                // numbers
                let value = result[i].parse::<u8>().unwrap();
                if value < 10 {
                    processed.push(result[i].clone());
                    prev_number_pos = (processed.len() - 1) as i32;
                } else {
                    if !prev_run_explode {
                        // split
                        let left = value / 2;
                        let right = (value + 1) / 2;
                        processed.push("[".to_string());
                        processed.push(left.to_string());
                        processed.push(",".to_string());
                        processed.push(right.to_string());
                        processed.push("]".to_string());
                        for j in i + 1..result.len() {
                            processed.push(result[j].clone());
                        }

                        //println!("split {}", value);
                        reduced = true;
                        can_split = false;
                        break;
                    } else  {
                        can_split = true;
                        processed.push(result[i].clone());
                        prev_number_pos = (processed.len() - 1) as i32;
                    }
                }
            }
            i += 1;
        }

        prev_run_explode = curr_run_explode;
        //processed.iter().for_each(|s| print!("{}", s));
        //println!("");
        result = processed.clone();
    }

    result
}

fn magnitude(input: &Vec<String>) -> u64  {
    let mut result: Vec<String> = Vec::new();
    let mut processed: Vec<String> = Vec::new();
    //println!("============= magnitude =================");

    result.extend(input.clone());

    //result.iter().for_each(|s| print!("{}", s));
    //println!("");

    let mut reduced: bool = true;

    while reduced {
        reduced = false;
        let mut i: usize = 0;
        processed.clear();

        while i < result.len() {
            if result[i] == "[" {
                if result[i+1] == "[" {
                    processed.push(result[i].clone());
                } else if result[i+3] != "[" {
                    // compute
                    let left = result[i + 1].to_string().parse::<u64>().unwrap();
                    let right = result[i + 3].to_string().parse::<u64>().unwrap();
                    processed.push((3 * left+ 2 * right).to_string());

                    for j in i + 5..result.len() {
                         processed.push(result[j].clone());
                    }
                    reduced = true;
                    break;
                } else  {
                    processed.push(result[i].clone());
                }
            } else {
                processed.push(result[i].clone());
            }
            i += 1;
        }
        result = processed.clone();

        //result.iter().for_each(|s| print!("{}", s));
        //println!("");
    }

    result[0].parse::<u64>().unwrap()
}

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let mut list:Vec<String> = lines[0].chars().map(|c| c.to_string()).collect();
    for i in 1..lines.len()  {
        let input2: Vec<String> = lines[i].chars().map(|c| c.to_string()).collect();
        list = add(&list, &input2);

        list.iter().for_each(|s| print!("{}", s));
        println!("");
    }
    let m = magnitude(&list);

    println!("magnitude {}", m);

}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let mut result: u64 = 0;

    for i in 0..lines.len()-1  {
        for j in i+1..lines.len() {
            let input1: Vec<String> = lines[i].chars().map(|c| c.to_string()).collect();
            let input2: Vec<String> = lines[j].chars().map(|c| c.to_string()).collect();

            let add1 = add(&input1, &input2);
            let m1 = magnitude(&add1);
            if m1 > result {
                result = m1
            }

            let add2 = add(&input2, &input1);
            let m2 = magnitude(&add2);
            if m2 > result {
                result = m2;
            }
            print!("({}, {})", i, j);
        }
        println!("");
    }
    println!("result {}", result);
}

}
