pub mod day20 {

fn calculate(lines :&Vec<String>, steps: usize)->u64  {
    let mut enhancement: Vec<char> = lines[0].chars().collect();

    let mut input_image: Vec<Vec<u8>> = Vec::new();

    let mut image_width = lines[2].len();
    let mut image_height = lines.len() - 2;

    input_image.push(vec![0;image_width+4]);
    input_image.push(vec![0;image_width+4]);

    for i in 2..lines.len()  {
        let mut curr_input: Vec<u8> = Vec::new();
        curr_input.extend(vec![0;2]);
        let binary_input: Vec<u8> = lines[i].chars().map(|c| if c == '.' {0} else {1}).collect();
        curr_input.extend(binary_input);
        curr_input.extend(vec![0;2]);
        input_image.push(curr_input.clone());
    }

    input_image.push(vec![0;image_width+4]);
    input_image.push(vec![0;image_width+4]);

    let neighbours: Vec<(i32, i32)> = vec![(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1)];
    let mut light_pixels: u64 = 0;
    let f0:u8 = if enhancement[0] == '#'  {1} else {0};
    let f1:u8 = if enhancement[511] == '#'  {1} else {0};

    for s in 0..steps {
        //println!("============================================================");
        let mut new_input_image: Vec<Vec<u8>> = Vec::new();
        light_pixels = 0;
        let f = if s % 2 == 0  {f0} else {f1};

        new_input_image.push(vec![f;image_width+6]);
        new_input_image.push(vec![f;image_width+6]);

        for y in 1..image_height + 3 {
            let mut curr_new_input: Vec<u8> = Vec::new();
            curr_new_input.extend(vec![f;2]);

            for x in 1..image_width + 3 {
                let e_pos: usize = neighbours.iter().fold(0, |r, (dy, dx)| {
                    let xx: usize = (x as i32 + *dx) as usize;
                    let yy: usize = (y as i32 + *dy) as usize;
                    r * 2 + input_image[yy][xx] as usize
                });
                if enhancement[e_pos as usize] == '#' {
                    light_pixels += 1;
                    curr_new_input.push(1);
                    //print!("#");
                } else {
                    curr_new_input.push(0);
                    //print!(".");
                }
            }
            //println!("");
            curr_new_input.extend(vec![f;2]);
            new_input_image.push(curr_new_input);
        }

        new_input_image.push(vec![f;image_width+6]);
        new_input_image.push(vec![f;image_width+6]);
        image_width += 2;
        image_height+= 2;
        input_image = new_input_image;
        //println!("number of light pixels: {}", light_pixels);
    }

    light_pixels
}


pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let steps = 2;
    let light_pixels = calculate(&lines, steps);
    println!("number of light pixels: {} after {} steps.", light_pixels, steps);
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let steps = 50;
    let light_pixels = calculate(&lines, steps);
    println!("number of light pixels: {} after {} steps.", light_pixels, steps);
}


}
