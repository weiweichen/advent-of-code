pub mod day17 {

fn hit((sx, sy): &(i32, i32), x_range: &(i32, i32), y_range:&(i32, i32)) -> bool  {
    let mut curr_sx = *sx;
    let mut curr_sy = *sy;
    let mut curr_x: i32 = 0;
    let mut curr_y: i32 = 0;

    while !(curr_x > x_range.1 || curr_y < y_range.0)  {
        curr_x += curr_sx;
        curr_y += curr_sy;
        if curr_sx > 0  {
            curr_sx -= 1;
        }
        curr_sy -= 1;
        if curr_x >= x_range.0 && curr_x <= x_range.1 && curr_y >= y_range.0 && curr_y <= y_range.1  {
            return true
        }
    }

    false
}

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let tmp: String = lines[0].chars().collect::<Vec<char>>().iter().rev()
        .take(lines[0].len()-13).rev().collect::<Vec<&char>>().iter().map(|c| **c).collect::<Vec<char>>()
        .iter().collect::<String>();

    let tmp_split: Vec<&str> = tmp.split(", ").collect();

    let x_range_str_tmp: String = tmp_split[0].chars().collect::<Vec<char>>().iter().rev()
        .take(tmp_split[0].len() - 2).rev().collect::<Vec<&char>>().iter().map(|c| **c).collect::<Vec<char>>()
        .iter().collect::<String>();

    let x_range_str: Vec<&str> = x_range_str_tmp.split("..").collect::<Vec<&str>>();

    let y_range_str_tmp: String = tmp_split[1].chars().collect::<Vec<char>>().iter().rev()
        .take(tmp_split[1].len() - 2).rev().collect::<Vec<&char>>().iter().map(|c| **c).collect::<Vec<char>>()
        .iter().collect::<String>();

    let y_range_str: Vec<&str> = y_range_str_tmp.split("..").collect::<Vec<&str>>();

    let x_range: (i32, i32) = (x_range_str[0].parse::<i32>().unwrap(), x_range_str[1].parse::<i32>().unwrap());
    let y_range: (i32, i32) = (y_range_str[0].parse::<i32>().unwrap(), y_range_str[1].parse::<i32>().unwrap());
    println!("x = {:#?}, y = {:#?}", x_range, y_range);
    println!("result {}", ((y_range.0 + 1) * y_range.0) / 2);

}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let tmp: String = lines[0].chars().collect::<Vec<char>>().iter().rev()
        .take(lines[0].len()-13).rev().collect::<Vec<&char>>().iter().map(|c| **c).collect::<Vec<char>>()
        .iter().collect::<String>();

    let tmp_split: Vec<&str> = tmp.split(", ").collect();

    let x_range_str_tmp: String = tmp_split[0].chars().collect::<Vec<char>>().iter().rev()
        .take(tmp_split[0].len() - 2).rev().collect::<Vec<&char>>().iter().map(|c| **c).collect::<Vec<char>>()
        .iter().collect::<String>();

    let x_range_str: Vec<&str> = x_range_str_tmp.split("..").collect::<Vec<&str>>();

    let y_range_str_tmp: String = tmp_split[1].chars().collect::<Vec<char>>().iter().rev()
        .take(tmp_split[1].len() - 2).rev().collect::<Vec<&char>>().iter().map(|c| **c).collect::<Vec<char>>()
        .iter().collect::<String>();

    let y_range_str: Vec<&str> = y_range_str_tmp.split("..").collect::<Vec<&str>>();

    let x_range: (i32, i32) = (x_range_str[0].parse::<i32>().unwrap(), x_range_str[1].parse::<i32>().unwrap());
    let y_range: (i32, i32) = (y_range_str[0].parse::<i32>().unwrap(), y_range_str[1].parse::<i32>().unwrap());
    println!("x = {:#?}, y = {:#?}", x_range, y_range);

    let mut start_x:i32 = 0;
    for x in 0..x_range.0  {
        if x * (x + 1)/2 >= x_range.0  {
            start_x = x;
            break;
        }
    }

    let mut hits: u64 = 0;
    for x in start_x..x_range.1 + 1 {
        for y in y_range.0.. -y_range.0 {
            if hit(&(x, y), &x_range, &y_range)  {
                hits += 1;
            }
        }
    }
    println!("num hits {}", hits);

}

}
