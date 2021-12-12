pub mod day8 {
pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let segments: Vec<(Vec<&str>, Vec<&str>)> = lines.iter().
        map(|line| line.split("|").collect()).
        map(|segments: Vec<&str>|(segments[0].split_whitespace().collect(), segments[1].split_whitespace().collect())).
        collect();

    segments.iter().for_each(|(s1, s2)| {
        println!("input: {:?}", s1);
        println!("output: {:?}", s2);
    });

    let result = segments.iter().fold(0, |sum, (s1, s2)| {
        //println!("output: {:?}", s2);
        let s = s2.iter().fold(0, |curr_sum, output| {
            if output.len() == 2 || output.len() == 4 || output.len() == 3 || output.len() == 7
            {curr_sum + 1} else  {curr_sum}
        });
        // println!("curr_sum {}", s );
        sum + s});
    println!("num inputs {}, result {}", lines.len(), result);
}

fn contains(a: &String, b: &String)->bool  {
    let a_chars: Vec<char> = a.chars().collect();
    let mut b_chars: Vec<char> = b.chars().collect();
    // println!("{:?}  contains {:?}", a_chars, b_chars);
    let b_set: std::collections::HashSet<char> = b_chars.drain(..).collect();
    return a_chars.iter().fold(true, |r, c| r && b_set.contains(c));
}

fn infer_map(inputs: &Vec<String>)->std::collections::HashMap<String, i32>  {
    let mut result: std::collections::HashMap<String, i32> = std::collections::HashMap::new();
    result.entry(inputs[0].clone()).or_insert(1);
    result.entry(inputs[1].clone()).or_insert(7);
    result.entry(inputs[2].clone()).or_insert(4);
    result.entry(inputs[9].clone()).or_insert(8);

    for i in 3..=5  {
        if contains(&inputs[0], &inputs[i])  {
            result.entry(inputs[i].clone()).or_insert(3);
        }
    }

    let mut six: String = "".to_string();
    for i in 6..=8  {
        if !contains(&inputs[1], &inputs[i])  {
            result.entry(inputs[i].clone()).or_insert(6);
            six = inputs[i].clone();
        }
        if contains(&inputs[2], &inputs[i]) {
            result.entry(inputs[i].clone()).or_insert(9);
        }
    }

    for i in 3..=5  {
        if contains(&inputs[i], &six)  {
            result.entry(inputs[i].clone()).or_insert(5);
        }
    }

    for i in 3..=5  {
        if !result.contains_key(&inputs[i])  {
            result.entry(inputs[i].clone()).or_insert(2);
        }
    }

    for i in 6..=8  {
        if !result.contains_key(&inputs[i])  {
            result.entry(inputs[i].clone()).or_insert(0);
        }
    }

    //for (key, value) in &result  {
    //   println!("key {}, value {}", key, value);
    //}
    result
}

fn compute_output(input_map: &std::collections::HashMap<String, i32>, outputs: &Vec<String>)->i32  {
    outputs.iter().fold(0 as i32, |r, s| {
        let v = match input_map.get(s) {
            Some(&number) => number,
            _ => 0,
        };
        r * 10 + v
    })
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let segments: Vec<(Vec<String>, Vec<String>)> = lines.iter().
        map(|line| line.split("|").collect()).
        map(|segments: Vec<&str>|(segments[0].split_whitespace().collect(), segments[1].split_whitespace().collect())).
        map(|(input, output) : (Vec<&str>, Vec<&str>) | {
            let mut c = input;
            c.sort_by(|&a, &b| a.len().cmp(&b.len()));
            (c, output)
        }).
        map(|(input, output)| {
            (input.iter().map(|&s|{
                let mut r : Vec<char> = s.chars().collect();
                r.sort();
                r.into_iter().collect()}).collect(),

            output.iter().map(|&s| {
                let mut r : Vec<char> = s.chars().collect();
                r.sort();
                r.into_iter().collect()}).collect(),
            )
        }).collect();

    // segments.iter().for_each(|(s1, s2)| {
    //     println!("input: {:?}", s1);
    //     println!("output: {:?}", s2);
    // });


    let result = segments.iter().fold(0, |curr_sum, (inputs, outputs)|  {
        let r = compute_output(&infer_map(inputs), outputs);
        println!("output {}", r);
        curr_sum + r
    });

    println!("num inputs {}, result {}", lines.len(), result);
}

}
