pub mod day5 {

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let vents: Vec<((i32, i32), (i32, i32))> =
       lines.iter().map(|line| line.split_whitespace().collect()).map(|v: Vec<&str>| (v[0].split(",").map(|n| n.parse::<i32>().unwrap()).collect(),
                                                                                      v[2].split(",").map(|n| n.parse::<i32>().unwrap()).collect())).
           map(|(v1, v2): (Vec<i32>, Vec<i32>)| ((v1[0], v1[1]), (v2[0], v2[1]))).collect();

    let mut point_cnt_map: std::collections::HashMap<(i32, i32), i32> = std::collections::HashMap::new();
    vents.iter().for_each(|((v1x, v1y), (v2x, v2y))| {
        let dx = if v1x < v2x  {1} else {-1};
        let dy = if v1y < v2y  {1} else {-1};
        if v1x == v2x  {
            for i in 0..=(v1y - v2y).abs(){
                *point_cnt_map.entry((*v1x, *v1y + i * dy)).or_insert(0) += 1;
            }
        } else if v1y == v2y  {
            for i in 0..=(v1x - v2x).abs(){
                *point_cnt_map.entry((*v1x + i * dx, *v1y)).or_insert(0) += 1;
            }
        }
    });

    let sum = point_cnt_map.iter().fold(0, |sum, ((_, _), cnt)| sum + if *cnt >= 2 {1} else {0});
    println!("result: {}", sum);
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let vents: Vec<((i32, i32), (i32, i32))> =
        lines.iter().map(|line| line.split_whitespace().collect()).map(|v: Vec<&str>| (v[0].split(",").map(|n| n.parse::<i32>().unwrap()).collect(),
                                                                                       v[2].split(",").map(|n| n.parse::<i32>().unwrap()).collect())).
            map(|(v1, v2): (Vec<i32>, Vec<i32>)| ((v1[0], v1[1]), (v2[0], v2[1]))).collect();

    let mut point_cnt_map: std::collections::HashMap<(i32, i32), i32> = std::collections::HashMap::new();
    vents.iter().for_each(|((v1x, v1y), (v2x, v2y))| {
        let dx = if v1x < v2x  {1} else {-1};
        let dy = if v1y < v2y  {1} else {-1};
        if v1x == v2x  {
            for i in 0..=(v1y - v2y).abs(){
                *point_cnt_map.entry((*v1x, *v1y + i * dy)).or_insert(0) += 1;
            }
        } else if v1y == v2y  {
            for i in 0..=(v1x - v2x).abs(){
                *point_cnt_map.entry((*v1x + i * dx, *v1y)).or_insert(0) += 1;
            }
        } else if (v1x - v2x).abs() == (v1y - v2y).abs() {
            for i in 0..=(v1x-v2x).abs()  {
                *point_cnt_map.entry((*v1x + i * dx, *v1y + i * dy)).or_insert(0) += 1;
            }
        }
    });

    let sum = point_cnt_map.iter().fold(0, |sum, ((_, _), cnt)| sum + if *cnt >= 2 {1} else {0});
    println!("result: {}", sum);
}
}
