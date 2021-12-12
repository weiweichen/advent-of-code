pub mod day11 {
    fn neighbours(r: usize, c: usize, num_rows: usize, num_cols: usize) -> Vec<(usize, usize)> {
        let mut result: Vec<(usize, usize)> = Vec::new();
        let dx_dy = vec![(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];
        for i in 0..dx_dy.len() {
            let curr_r = r as i32 - dx_dy[i].0;
            let curr_c = c as i32 - dx_dy[i].1;
            if curr_r >= 0 && curr_r < num_rows as i32 && curr_c >= 0 && curr_c < num_cols as i32 {
                result.push((curr_r as usize, curr_c as usize));
            }
        }
        result
    }

    fn print(input: &Vec<Vec<i8>>) {
        println!("====================================");
        input.iter().for_each(|i|
            println!("{:?}", i)
        );
    }


    fn step(input: &mut Vec<Vec<i8>>) -> i32 {
        let num_rows = input.len();
        let num_cols = input[0].len();

        let mut new_tens: i32 = 0;
        for i in 0..input.len() {
            for j in 0..input[0].len() {
                if input[i][j] < 10 {
                    input[i][j] += 1;
                }
                if input[i][j] == 10 {
                    new_tens += 1;
                }
            }
        }

        let mut done: bool = new_tens == 0;

        let mut lighted: std::collections::HashSet<(usize, usize)> = std::collections::HashSet::new();
        while !done {
            new_tens = 0;

            for i in 0..input.len() {
                for j in 0..input[0].len() {
                    if input[i][j] == 10 && !lighted.contains(&(i, j)) {
                        let nps = neighbours(i, j, num_rows, num_cols);
                        nps.iter().for_each(|(x, y)| {
                            if input[*x][*y] < 10 {
                                input[*x][*y] += 1;
                                if input[*x][*y] == 10 {
                                    new_tens += 1;
                                }
                            }
                        });
                        lighted.insert((i, j));
                    }
                }
            }

            done = new_tens == 0;
        }

        // update back to 0
        let mut num_flashes: i32 = 0;
        for i in 0..input.len() {
            for j in 0..input[0].len() {
                if input[i][j] == 10 {
                    input[i][j] = 0;
                    num_flashes += 1;
                }
            }
        }
        num_flashes
    }

    pub fn part1<R: std::io::BufRead>(reader: R) {
        let lines: Vec<String> = reader
            .lines()
            .map(|line| line.unwrap())
            .collect();

        let mut energy: Vec<Vec<i8>> = lines.iter().map(|line| line.chars().map(|c| c.to_string().parse::<i8>().unwrap())
            .collect()).collect();

        print(&energy);
        let mut num_flashes: i32 = 0;

        let total_steps = 100;

        for s in 0..total_steps {
            num_flashes += step(&mut energy);
        }

        println!("after {} steps, total number of flashes {}", total_steps, num_flashes);
    }

    pub fn part2<R: std::io::BufRead>(reader: R) {
        let lines: Vec<String> = reader
            .lines()
            .map(|line| line.unwrap())
            .collect();
        let mut energy: Vec<Vec<i8>> = lines.iter().map(|line| line.chars().map(|c| c.to_string().parse::<i8>().unwrap())
            .collect()).collect();

        print(&energy);
        let mut num_flashes: i32 = 0;

        let mut done: bool = false;
        let mut total_steps: i32 = 0;

        while !done {
            total_steps += 1;
            num_flashes = step(&mut energy);
            if num_flashes == (energy.len() * energy[0].len()) as i32 {
                done = true;
            }
        }

        println!("after {} steps, total number of flashes {}", total_steps, num_flashes);
    }
}
