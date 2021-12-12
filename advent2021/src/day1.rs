pub mod day1 {

pub fn day1_part1<R: std::io::BufRead>(reader:R) {
    let r = reader;
    let numbers: Vec<i64> = r
        .lines()
        .map(|line| line.unwrap().parse::<i64>().unwrap())
        .collect();

    println!("Vector length  {}", numbers.len());
    let mut cnt = 0;
    let mut icnt = 0;
    let mut prev_value = &numbers[0];

    for number in numbers.iter()  {
        match number  {
            _ => {
                cnt = cnt + 1;
                if number > prev_value  {
                    icnt = icnt + 1;
                }
                prev_value = number;
            }
        }
    }
    println!("total count {}, icnt {}", cnt, icnt);
}

pub fn day1_part2<R: std::io::BufRead>(reader:R) {
    let numbers: Vec<i64> = reader
        .lines()
        .map(|line| line.unwrap().parse::<i64>().unwrap())
        .collect();

    println!("Vector length  {}", numbers.len());

    let mut prev_sum = &numbers[0] + &numbers[1] + &numbers[2];
    let mut scnt = 0;

    for i in 0..numbers.len()-2  {
        let curr_sum = &numbers[i] + &numbers[i + 1] + &numbers[i + 2];
        if curr_sum > prev_sum  {
            scnt = scnt + 1;
        }
        prev_sum = curr_sum;
    }

    println!("total scnt {}", scnt);
}

}