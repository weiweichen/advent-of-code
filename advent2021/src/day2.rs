pub mod day2 {

pub fn day2_part1<R: std::io::BufRead>(reader:R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let mut h_value:i64 = 0;
    let mut v_value:i64 = 0;

    for line in lines.iter()  {
        let strs: Vec<&str> = line.split(" ").collect();
        let value: i64 = strs[1].parse::<i64>().unwrap();
        match strs[0] {
            "down" =>  {
                v_value += value;
            }
            "up" =>  {
                v_value -= value;
            }

            "forward" =>  {
                h_value += value;
            }

            "backward" =>  {
                h_value += value;
            }
            _ =>  {}
        }

        println!("{} {} {} {}", strs[0], value, v_value, h_value);
    }
    println!("Vector length {} {}", lines.len(), h_value * v_value);


}

pub fn day2_part2<R: std::io::BufRead>(reader:R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let mut depth:i64 = 0;
    let mut h_position:i64 = 0;
    let mut aim:i64 = 0;

    for line in lines.iter()  {
        let strs: Vec<&str> = line.split(" ").collect();
        let value: i64 = strs[1].parse::<i64>().unwrap();
        match strs[0] {
            "down" =>  {
                aim += value;
            }
            "up" =>  {
                aim -= value;
            }

            "forward" =>  {
                h_position += value;
                depth += aim * value;
            }

            _ =>  {
                unreachable!();
            }
        }

        println!("{} {} a:{} d:{} h:{}", strs[0], value, aim, depth, h_position);
    }


    println!("Vector length {} {}", lines.len(), depth * h_position);
}

}