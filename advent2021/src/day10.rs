pub mod day10 {

pub fn is_open(c: &char) ->bool {
    match c  {
        '(' | '{' | '<' | '[' => true,
        _ => false
    }
}

pub fn is_close(c: &char)->bool  {
    match c  {
        ')' | '}' | '>' | ']' => true,
        _ => false
    }
}

pub fn get_close(c: &char)->char{
    match c  {
        '(' => ')',
        '{' => '}',
        '[' => ']',
        '<' => '>',
        _ => '*'
    }
}

pub fn matches(c1: &char, c2: &char) -> bool  {
    match (c1, c2) {
        ('(', ')') => { true },
        ('{', '}') =>  {true},
        ('[', ']') =>  {true},
        ('<', '>') =>  {true},
        _ =>  {false},
    }
}

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let line_chars: Vec<Vec<char>> = lines.iter().map(|line| line.chars().collect()).collect();
    let mut result: Vec<char> = Vec::new();

    let results: Vec<char> = line_chars.iter().map( |line_char|  {
        let mut curr: Vec<char> = Vec::new();
        let mut v: char = '*';
        for i in 0..line_char.len()  {
            if is_open(&line_char[i])  {
                curr.push(line_char[i]);
            } else if i > 0 &&  matches(&curr[curr.len() - 1], &line_char[i]) {
                curr.pop();
            } else  {
                v = line_char[i];
                break;
            }
        }
        v
    }).collect();

    let sum_value = results.iter().fold(0, |sum, &c|  {
       match c {
           ')' => sum + 3,
           '}' => sum + 1197,
           '>' => sum + 25137,
           ']' => sum + 57,
           _ => sum ,
       }
    });

    println!("{:?}", results);
    println!("sum: {}", sum_value);
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let line_chars: Vec<Vec<char>> = lines.iter().map(|line| line.chars().collect()).collect();
    let incomplete_lines: Vec<&Vec<char>> = line_chars.iter().filter( |line_char| {
        let mut curr: Vec<char> = Vec::new();
        let mut incomplete: bool = true;
        for i in 0..line_char.len()  {
            if is_open(&line_char[i])  {
                curr.push(line_char[i]);
            } else if i > 0 &&  matches(&curr[curr.len() - 1], &line_char[i]) {
                curr.pop();
            } else  {
                incomplete = false;
                break;
            }
        }
        incomplete
    }).collect();

    //incomplete_lines.iter().for_each( |line|  {
    //    println!("{:?}", line);
    //});

    let to_complete: Vec<Vec<char>> = incomplete_lines.iter().map( |line|  {
        let mut curr: Vec<char> = Vec::new();
        let mut incomplete: bool = true;
        for i in 0..line.len()  {
            if is_open(&line[i])  {
                curr.push(line[i]);
            } else if i > 0 &&  matches(&curr[curr.len() - 1], &line[i]) {
                curr.pop();
            }
        }
        curr.iter().map(|c| get_close(c)).collect::<Vec<char>>().into_iter().rev().collect()
    }).collect();

    //to_complete.iter().for_each( |line|  {
    //    println!("{:?}", line);
    //});
    let mut scores : Vec<u64> = to_complete.iter().map(|line|{
       line.iter().fold(0, |sum, c|  {
           match c {
               ')' => sum * 5 + 1,
               '}' => sum * 5 + 3,
               '>' => sum * 5 + 4 ,
               ']' => sum * 5 + 2,
               _ => sum ,
           }
       } )
    }).collect();

    scores.sort();
    println!("scores {:?}", scores);
    println!("size: {}, result: {}", scores.len(), scores[scores.len() / 2]);
}
}
