pub mod day24 {
use std::collections::HashMap;

fn compute(result: &mut[i64; 4], ops: &Vec<(&str, usize, (bool, usize, i64))>, pc: usize, input: Option<i64>)-> bool {
    let (op, idx_a, (is_var, idx_b, value_b)) = ops[pc];
    let mut b: i64 = result[idx_b];
    if !is_var {
        b = value_b;
    }

    match ops[pc].0  {
        "inp" =>  {
            result[0] = input.unwrap();
        },
        "add" =>  {
            result[idx_a] += b;
        },
        "mul" =>  {
            let a = result[idx_a];
            result[idx_a] = a * b;
        },
        "div" =>  {
            if b == 0  {
                panic!("div by 0");
            } else {
                let d: f64 = result[idx_a] as f64 / b as f64;
                result[idx_a] = d.trunc() as i64;
            }
        },
        "mod" =>  {
            if result[idx_a] < 0 || b <= 0  {
                panic!("illegal mod");
            }
            result[idx_a] %= b;
        },
        "eql" =>  {
            if result[idx_a] == b  {
                result[idx_a] = 1;
            } else  {
                result[idx_a] = 0;
            }
        }
        _ => {}
    }
    true
}

fn solve(reg: [i64;4], ops: &Vec<(&str, usize, (bool, usize, i64))>, pc: usize,
         cache: &mut HashMap<([i64;4], usize), Option<i64>>,
         part2: bool) -> Option<i64> {
    if let Some(answer) = cache.get(&(reg, pc))  {
        return *answer;
    }

    let range = if part2  {
        [1, 2, 3, 4, 5, 6, 7, 8, 9]
    } else  {
        [9, 8, 7, 6, 5, 4, 3, 2, 1]
    };

    for inp in range  {
        let mut reg = reg;
        let mut pc = pc;

        compute(&mut reg, ops, pc, Some(inp));
        pc += 1;

        let mut to_cont: bool = false;
        while pc < 252 {
            if ops[pc].0 == "inp" {
                if let Some(v) = solve(reg, ops, pc, cache, part2) {
                    cache.insert((reg, pc), Some(v * 10 + inp));
                    return Some(v * 10 + inp);
                } else {
                    to_cont = true;
                    break;
                }
            } else {
                compute(&mut reg, ops, pc, None);
                pc += 1;
            }
        }
        if to_cont {
            continue;
        }

        if reg[3] == 0  {
            cache.insert((reg, pc), Some(inp));
            return Some(inp);
        }
    }

    cache.insert((reg, pc), None);
    None
}

fn parse_ops(lines: &Vec<String>)->Vec<(&str, usize, (bool, usize, i64))>  {
    lines.iter().map(|line| {
        let s: Vec<&str> = line.split(" ").collect();
        match s[0]  {
            "inp" =>  {
                (s[0], 0, (false, 0, 0))
            },
            _ => {
                let mut var_a: usize = 0;
                match s[1]  {
                    "x" => { var_a = 1;},
                    "y" => { var_a = 2;},
                    "z" => { var_a = 3;},
                    "w" => { var_a = 0;},
                    _ =>  {},
                }

                let is_var_b = s[2] == "x" || s[2] == "y" || s[2] == "z" || s[2] == "w";
                let mut var_b: usize = 0;
                let mut value_b: i64 = 0;
                if !is_var_b {
                    value_b = s[2].parse::<i64>().unwrap();
                } else  {
                    match s[2]  {
                        "x" => { var_b = 1;},
                        "y" => { var_b = 2;},
                        "z" => { var_b = 3;},
                        "w" => { var_b = 0;},
                        _ =>  {},
                    }
                }

                (s[0], var_a, (is_var_b, var_b, value_b))
            }
        }

    }).collect()
}

pub fn part1<R: std::io::BufRead>(reader: R) {
        let lines: Vec<String> = reader
            .lines()
            .map(|line| line.unwrap())
            .collect();

    let ops = parse_ops(&lines);
    println!("size of input ops: {}", ops.len());

    let mut cache = HashMap::new();
    let answer = solve([0;4], &ops, 0, &mut cache, false);

    let result = format!("{}", answer.unwrap()).chars().rev().collect::<String>();
    println!("result: {}", result);
}


pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let ops = parse_ops(&lines);
    println!("size of input ops: {}", ops.len());

    let mut cache = HashMap::new();
    let answer = solve([0;4], &ops, 0, &mut cache, true);

    let result = format!("{}", answer.unwrap()).chars().rev().collect::<String>();
    println!("result: {}", result);
}

}
