pub mod day12 {

fn is_small_cave(cave: &str) -> bool {
    cave !="start" && cave.to_lowercase() == cave
}

fn explore_part1(path: Vec<&str>, curr_end: &str, results:& mut Vec<Vec<String>>, graph: &std::collections::HashMap<&str, Vec<&str>>)  {
    if curr_end == "end"  {
        let p: Vec<String> = path.iter().map(|p| p.to_string()).collect();
        results.push(p);
    } else  {
        let neighbours = match graph.get(curr_end) {
            Some(v) => v,
            None =>  {
                panic!("you impossible");
            }
        };
        neighbours.iter().for_each(|n| {
            let mut new_path = path.clone();
            new_path.push(curr_end);

            if is_small_cave(n)  {
                if !path.contains(n)  {
                    explore_part1(new_path, n, results, graph);
                }
            } else  {
                explore_part1(new_path, n, results, graph);
            }
        });
    }
}

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let paths: Vec<(&str, &str)> = lines.iter().map(|line| {
        let s: Vec<&str> = line.split("-").collect();
        (s[0], s[1])
    }).collect();

    let mut graph: std::collections::HashMap<&str, Vec<&str>> = std::collections::HashMap::new();

    paths.iter().for_each(|(src, dst)| {
        let v1 = graph.entry(src).or_insert(Vec::new());
        v1.push(dst);
        let v2 = graph.entry(dst).or_insert(Vec::new());
        v2.push(src);

    });

    for (key, value) in graph.iter() {
        println!("{} / {:?}", key, value);
    }

    let mut results: Vec<Vec<String>> = Vec::new();
    let path: Vec<&str> = Vec::new();
    explore_part1(path, "start", &mut results, &graph);

    println!("results size: {}", results.len());

}

fn explore_part2(path: Vec<&str>, curr_end: &str,
                 results:& mut Vec<Vec<String>>,
                 visited_twice: bool,
                 graph: &std::collections::HashMap<&str, Vec<&str>>)  {
    if curr_end == "end"  {
        let p: Vec<String> = path.iter().map(|p| p.to_string()).collect();
        // println!("found path: {:?} ", p );
        results.push(p);
    } else  {
        let neighbours = match graph.get(curr_end) {
            Some(v) => v,
            None =>  {
                panic!("you impossible");
            }
        };
        neighbours.iter().for_each(|n| {
            let mut new_path = path.clone();
            new_path.push(curr_end);
            if is_small_cave(n) {
                if !path.contains(n) {
                    explore_part2(new_path, n, results, visited_twice, graph);
                } else if !visited_twice {
                    explore_part2(new_path, n, results, true, graph);
                }
            } else if n != &"start" {
                explore_part2(new_path, n, results, visited_twice, graph);
            }
        });
    }
}


pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let paths: Vec<(&str, &str)> = lines.iter().map(|line| {
        let s: Vec<&str> = line.split("-").collect();
        (s[0], s[1])
    }).collect();

    let mut graph: std::collections::HashMap<&str, Vec<&str>> = std::collections::HashMap::new();

    paths.iter().for_each(|(src, dst)| {
        let v1 = graph.entry(src).or_insert(Vec::new());
        v1.push(dst);
        let v2 = graph.entry(dst).or_insert(Vec::new());
        v2.push(src);

    });

    for (key, value) in graph.iter() {
        println!("{} / {:?}", key, value);
    }

    let mut results: Vec<Vec<String>> = Vec::new();
    let path: Vec<&str> = Vec::new();
    explore_part2(path, "start", &mut results, false,  &graph);

    println!("results size: {}", results.len());
}

}
