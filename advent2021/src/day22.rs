pub mod day22 {

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let inputs: Vec<(&str, (i32, i32), (i32, i32), (i32, i32))> = lines.iter().map(|line| {
        let s: Vec<&str> = line.split(" ").collect();
        let action = s[0];

        let x_y_z: Vec<(i32, i32)> = s[1].split(",").collect::<Vec<&str>>().iter().map(|coords| {
            let c = coords.split("=").collect::<Vec<&str>>()[1].split("..").collect::<Vec<&str>>();
            (c[0].parse::<i32>().unwrap(), c[1].parse::<i32>().unwrap())
        }).collect();
        (action, x_y_z[0], x_y_z[1], x_y_z[2])
    }).collect();

    let mut cubes: std::collections::HashMap<(i32, i32, i32), bool> = std::collections::HashMap::new();

    for (action, (xs, xe), (ys, ye), (zs, ze)) in inputs.iter() {
        println!("{}, x={}..{}, y={}..{}, z={}..{}", action, xs, xe, ys, ye, zs, ze);

        if *xe < -50 || *xs > 50 || *ye <= -50 || *ys > 50 || *ze < -50 || *zs > 50 {
            continue;
        }
        for x in *xs..*xe + 1 {
            if x < -50 || x > 50 {
                continue;
            }
            for y in *ys..*ye + 1 {
                if y < -50 || y > 50 {
                    continue;
                }
                for z in *zs..*ze + 1 {
                    if z < -50 || z > 50 {
                        continue;
                    }
                    if *action == "on" {
                        let entry = cubes.entry((x, y, z)).or_insert(true);
                        *entry = true;
                    } else {
                        let entry = cubes.entry((x, y, z)).or_insert(false);
                        *entry = false;
                    }
                }
            }
        }
    }

    let mut num_oncubes: u64 = 0;
    for (key, value) in cubes {
        if value == true {
            num_oncubes += 1;
        }
    }
    println!("num_oncubes {}", num_oncubes);
}
    
// steal idea from https://www.reddit.com/r/adventofcode/comments/rlxhmg/2021_day_22_solutions/hpizza8/
pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let inputs: Vec<(&str, (i32, i32), (i32, i32), (i32, i32))> = lines.iter().map(|line| {
        let s: Vec<&str> = line.split(" ").collect();
        let action = s[0];

        let x_y_z: Vec<(i32, i32)> = s[1].split(",").collect::<Vec<&str>>().iter().map(|coords| {
            let c = coords.split("=").collect::<Vec<&str>>()[1].split("..").collect::<Vec<&str>>();
            (c[0].parse::<i32>().unwrap(), c[1].parse::<i32>().unwrap())
        }).collect();
        (action, x_y_z[0], x_y_z[1], x_y_z[2])
    }).collect();

    let mut cubes: std::collections::HashMap<((i32, i32), (i32, i32), (i32, i32)), i32> = std::collections::HashMap::new();
    // assumue the first input is always on action

    for i in 0..inputs.len() {
        let mut new_cubes: std::collections::HashMap<((i32, i32), (i32, i32), (i32, i32)), i32> = std::collections::HashMap::new();
        let action = inputs[i].0;
        let (xx0, xx1) = inputs[i].1;
        let (yy0, yy1) = inputs[i].2;
        let (zz0, zz1) = inputs[i].3;

        // cubes are always on
        for ((cx, cy, cz), v) in cubes.iter() {
            let x0 = std::cmp::max(cx.0, inputs[i].1.0);
            let x1 = std::cmp::min(cx.1, inputs[i].1.1);
            let y0 = std::cmp::max(cy.0, inputs[i].2.0);
            let y1 = std::cmp::min(cy.1, inputs[i].2.1);
            let z0 = std::cmp::max(cz.0, inputs[i].3.0);
            let z1 = std::cmp::min(cz.1, inputs[i].3.1);
            if x0 <= x1 && y0 <= y1 && z0 <= z1  {
                // overlaps
                let entry = new_cubes.entry(((x0, x1), (y0, y1), (z0, z1))).or_insert(0);
                *entry -= v;
            }
        }

        if action == "on"  {
            let entry = new_cubes.entry(((xx0, xx1), (yy0, yy1), (zz0, zz1))).or_insert(0);
            *entry += 1;
        }

        for (key, value) in new_cubes.iter()  {
            match cubes.get_mut(key)  {
                Some(v) => {
                    *v += value;
                },
                None => {
                    cubes.insert(*key,* value);
                },
            }
        }
    }

    let count: i64 = cubes.iter().fold(0, |s, (((x0, x1), (y0, y1), (z0, z1)), value)|  {
        s + (x1 - x0 + 1) as i64 * (y1 - y0 + 1) as i64 * (z1 - z0 + 1) as i64 * *value as i64
    });
    println!("count {}", count);

}
}
