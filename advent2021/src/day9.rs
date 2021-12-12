pub mod day9 {

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let heights: Vec<Vec<i32>> =
    lines.iter().map(|line| line.chars().collect::<Vec<char>>().iter().map(|c| c.to_string().parse::<i32>().unwrap()).collect()).collect();

    // heights.iter().for_each(|height| {
    //     println!("{:?}", height);
    // });

    let height_coords: Vec<Vec<(i32, (i32, i32))>>
    = heights.iter().enumerate().map(|(r, hs)| hs.iter().enumerate().map(|(c, &h)| (h, (r as i32, c as i32))).collect()).collect();

    let dx_dy :Vec<(i32, i32)>= vec![(0, -1), (0, 1), (-1, 0), (1, 0)];
    let num_rows : i32 = heights.len() as i32;
    let num_cols : i32 = heights[0].len() as i32;

    let sum = height_coords.iter().fold(0, |s, hcs| {
        let sub_sum = hcs.iter().fold(0, |ss, (h, (r, c))| {
            let points: Vec<i32> = dx_dy.iter().map(|(dx, dy)| {
                let rr = *r + *dx;
                let cc = *c + *dy;
                if rr < 0 || rr >= num_rows { 10 } else if cc < 0 || cc >= num_cols { 10 } else { heights[rr as usize][cc as usize] }
            }).collect();
            let min_value = points.iter().min();
            let cs = match min_value {
                Some(m) =>
                    if heights[*r as usize][*c as usize] < *m { heights[*r as usize][*c as usize] + 1 + ss } else { ss }
                _ => ss
            };
            // println!("( {:?} ({},  {})), {}", points, r, c, cs);
            cs
        });
        s + sub_sum
    });

    println!("num lines {}, result {}", heights.len(), sum);
}

fn neighbours(r: usize, c:usize, num_rows: usize, num_cols: usize)->Vec<(usize, usize)>  {
    let mut result: Vec<(usize, usize)> = Vec::new();
    //let dx_dy = vec![(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];
    let dx_dy = vec![(-1, 0), (0, -1), (0, 1), (1, 0)];
    for i in 0..dx_dy.len()  {
        let curr_r = r as i32 - dx_dy[i].0;
        let curr_c = c as i32 - dx_dy[i].1;
        if curr_r >=0 && curr_r < num_rows as i32 && curr_c >= 0 && curr_c < num_cols as i32 {
            result.push((curr_r as usize, curr_c as usize));
        }
    }
    result
}

fn basin_size(heights: &Vec<Vec<i32>>, r: usize, c: usize) -> i32  {
    let mut local_heights: Vec<Vec<i32>> = heights.clone();
    println!("basin_size for ({}, {})", r, c);

    let mut q: std::collections::VecDeque<(usize, usize)> = std::collections::VecDeque::new();
    let mut counted_set: std::collections::HashSet<(usize, usize)> = std::collections::HashSet::new();
    let mut result: i32 = 1;
    q.push_back((r, c));
    while !q.is_empty() {
        let mut curr_r: usize = 0;
        let mut curr_c: usize = 0;
        match q.front_mut()  {
            Some((r, c)) =>  {
                curr_r = *r;
                curr_c = *c;
            }
            _ =>  {}
        }
        let curr_h = local_heights[curr_r][curr_c];
        neighbours(curr_r, curr_c, heights.len(), heights[0].len()).iter().for_each(|(r, c)| {
            let n_h = local_heights[*r][*c];
            if n_h > curr_h  && n_h < 9 && !counted_set.contains(&(*r, *c)) {
                q.push_back((*r, *c));
                counted_set.insert((*r, *c));
                result += 1;
            }
        });
        local_heights[curr_r][curr_c] = 9;
        q.pop_front();
    }
    result
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();


    let heights: Vec<Vec<i32>> =
        lines.iter().map(|line| line.chars().collect::<Vec<char>>().iter().map(|c| c.to_string().parse::<i32>().unwrap()).collect()).collect();

    let num_rows : i32 = heights.len() as i32;
    let num_cols : i32 = heights[0].len() as i32;

    let mut results: Vec<i32> = Vec::new();
    for r in 0..num_rows as usize {
        for c in 0..num_cols as usize {
            let curr_h = heights[r][c];
            let get_size = neighbours(r as usize, c as usize, heights.len(), heights[0].len()).iter().fold( true, |ok, (rr, cc)| {
                let n_h = heights[*rr][*cc];
                if n_h <= curr_h  {
                    false
                } else  {
                    ok
                }
            });

            if get_size {
                results.push(basin_size(&heights, r, c));
            }
        }
    }



    // println!("basin_sizes {:?}, result {}", basin_size, basin_size[s-2] * basin_size[s - 1] * basin_size[s-3]);
    results.sort();
    let s = results.len();
    println!("basin_sizes {:?}, result {}", results, results[s-3] * results[s-2] * results[s-1]);
}


}
