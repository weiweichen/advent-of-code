pub mod day3 {

pub fn part1<R: std::io::BufRead>(reader:R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let len = lines[0].len();
    let mut cnts = vec![0; len];

    for line in lines.iter()  {
        for i in 0..len  {
            match line.chars().nth(i).unwrap() {
               '0' =>  {
                  cnts[i] += 1;
               }
               _ =>  {

               }
            }
        }
    }

    let mut gamma:i64 = 0;
    let mut epsilon:i64 = 0;
    let num_lines = lines.len();

    for cnt in cnts.iter()  {
        gamma = gamma * 2;
        epsilon = epsilon * 2;

        if cnt * 2 < num_lines {
            gamma = gamma  + 1;
        } else  {
            epsilon = epsilon + 1;
        }
    }

    let cnts_str: String = cnts.into_iter().map(|i| i.to_string()).collect::<String>();
    println!("Vector length {} {} g:{} e:{} g*e:{}", lines.len(), cnts_str, gamma, epsilon, gamma * epsilon);
}

fn num_zeros(lines: &Vec<String>, ith: usize) -> usize  {
    let mut cnt:usize = 0;

    for line in lines.iter()  {
            match line.chars().nth(ith).unwrap() {
                '0' =>  {
                    cnt += 1;
                }
                _ =>  {

                }
            }
        }
    cnt
}

fn to_number(line: &String)->i64  {
    let mut result: i64 = 0;
    for c in line.chars()  {
        result *= 2;
        match c  {
            '1' =>  {
                result += 1;
            }
            _ =>  {}
        }
    }
    result
}

pub fn part2<R: std::io::BufRead>(reader:R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let mut ith:usize = 0;
    let S: usize = 12;

    let mut og_lines = lines.clone();
    while ith < S && og_lines.len() > 1  {
        let num= num_zeros(&og_lines, ith);
        println!("num_zeros {}", num);
        let num_lines = og_lines.len();

        if num * 2 > num_lines {
            og_lines = og_lines.iter().filter(|line| line.chars().nth(ith).unwrap() == '0').cloned().collect::<Vec<String>>();
        } else  {
            og_lines = og_lines.iter().filter(|line| line.chars().nth(ith).unwrap() == '1').cloned().collect::<Vec<String>>();
        }

        ith += 1;
        //for l in og_lines.iter()  {
        //    println!("{}", l);
        //}
    }


    let mut co_lines = lines.clone();
    let mut ith:usize = 0;

    while ith < S && co_lines.len() > 1  {
        let num= num_zeros(&co_lines, ith);
        println!("== num_zeros {}", num);
        let num_lines = co_lines.len();

        if num * 2 > num_lines {
            co_lines = co_lines.iter().filter(|line| line.chars().nth(ith).unwrap() == '1').cloned().collect::<Vec<String>>();
        } else  {
            co_lines = co_lines.iter().filter(|line| line.chars().nth(ith).unwrap() == '0').cloned().collect::<Vec<String>>();
        }

        ith += 1;
        //for l in co_lines.iter()  {
        //    println!("== {}", l);
        //}
    }

    let og_number = to_number(&og_lines[og_lines.len() - 1]);
    let co_number = to_number(&co_lines[co_lines.len() - 1]);

    println!("{} og:{} co:{} r:{}", num_zeros(&lines, 0), og_number, co_number, og_number * co_number);

}

}