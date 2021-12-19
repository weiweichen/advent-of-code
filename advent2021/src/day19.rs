pub mod day19 {

use std::collections;

fn find_overlaps(scanner0: &Vec<Vec<i32>>, scanner1: &Vec<Vec<i32>>) -> (bool, (i32, i32, i32), Vec<usize>, Vec<i32>) {
    let mut overlaps: collections::HashMap<(i32, i32, i32), Vec<(&Vec<i32>, &Vec<i32>)>> = collections::HashMap::new();
    let permutes: Vec<Vec<usize>> = vec![
        vec![0, 1, 2], vec![0, 2, 1], vec![1, 0, 2], vec![1, 2, 0], vec![2, 0, 1], vec![2, 1, 0]];
    let directions: Vec<Vec<i32>> = vec![
        vec![1, 1, 1], vec![1, 1, -1], vec![1, -1, 1], vec![1, -1, -1],
        vec![-1, 1, 1], vec![-1, 1, -1], vec![-1, -1, 1], vec![-1, -1, -1]];

    for p in 0..permutes.len() {
        let permute = &permutes[p];
        for dd in 0..directions.len() {
            let dir = &directions[dd];
            overlaps.clear();

            for i in 0..scanner0.len()  {
                for j in 0..scanner1.len()  {
                    let p0 = &scanner0[i];
                    let p1 = &scanner1[j];

                    let d = (p0[0] - dir[0] * p1[permute[0]], p0[1] - dir[1] * p1[permute[1]], p0[2] - dir[2] * p1[permute[2]]);
                    let entry = overlaps.entry(d).or_insert(Vec::new());
                    entry.push((&p0, &p1));
                }
            }

            for (key, value) in &overlaps {
                if value.len() >= 12  {
                    return (true, *key, permute.to_vec(), dir.to_vec());
                }
            }
        }
    }


    (false, (0, 0, 0), vec![], vec![])
}

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let mut scanner_charts: Vec<Vec<Vec<i32>>> = Vec::new();
    let mut chars: Vec<Vec<i32>> = Vec::new();
    for i in 0..lines.len()  {
        if lines[i].len() == 0  {
            scanner_charts.push(chars.clone());
            continue;
        }

        let ch0 = lines[i].chars().nth(0).unwrap();
        let ch1 = lines[i].chars().nth(1).unwrap();
        if ch0 == '-' && ch1 == '-'  {
            chars.clear();
        } else  {
            let coords: Vec<i32> = lines[i].split(",").map(|p| p.parse::<i32>().unwrap()).collect();
            chars.push(coords.clone());
        }
    }

    let mut scanner_info: collections::HashMap<usize, (i32, i32, i32)> = collections::HashMap::new();
    let mut worker: collections::VecDeque<usize> = collections::VecDeque::new();
    worker.push_back(0);

    scanner_info.entry(0).or_insert((0, 0, 0));

    while !worker.is_empty() || scanner_info.len() < scanner_charts.len() {
        let mut i: usize = 0;
        if worker.is_empty()  {
            for i in 0..scanner_charts.len()  {
                if !scanner_info.contains_key(&i) {
                    worker.push_back(i);
                }
            }
        }

        match worker.front_mut() {
            Some(v) =>  {
               i = *v;
            },
            None => {}
        }
        worker.pop_front();

        let curr_offset = match scanner_info.get(&i)  {
            Some(v) => v.clone(),
            None =>  {
                scanner_info.entry(i).or_insert((0, 0, 0));
                (0, 0, 0)
            },
        };

        for j in 0..scanner_charts.len() {
            if scanner_info.contains_key(&j) || j == i {
                continue;
            }

            let (found, offset, perm, dir) = find_overlaps(&scanner_charts[i], &scanner_charts[j]);
            if found {
                println!("offset between({}, {}) is ({}, {}, {}), with perm {:?} and dir {:?})"
                         , i, j, offset.0, offset.1, offset.2, perm, dir);
                for c in 0..scanner_charts[j].len() {
                    let new_x = scanner_charts[j][c][perm[0]] * dir[0];
                    let new_y = scanner_charts[j][c][perm[1]] * dir[1];
                    let new_z = scanner_charts[j][c][perm[2]] * dir[2];
                    scanner_charts[j][c][0] = new_x;
                    scanner_charts[j][c][1] = new_y;
                    scanner_charts[j][c][2] = new_z;
                }
                let new_offset_x = curr_offset.0 + offset.0;
                let new_offset_y = curr_offset.1 + offset.1;
                let new_offset_z = curr_offset.2 + offset.2;

                worker.push_back(j);
                scanner_info.entry(j).or_insert((new_offset_x, new_offset_y, new_offset_z));
            }
        }
    }

    let mut beacons: collections::HashSet<(i32, i32, i32)> = collections::HashSet::new();
    for s in 0..scanner_charts.len()  {
        let curr_offset = match scanner_info.get(&s)  {
            Some(v) => v.clone(),
            None =>  {panic!("")},
        };

        println!("{}, curr offset({}, {}, {})", s, curr_offset.0, curr_offset.1, curr_offset.2);
        for l in 0..scanner_charts[s].len()  {
            let p =
                (scanner_charts[s][l][0] + curr_offset.0,
                                scanner_charts[s][l][1] + curr_offset.1,
                                scanner_charts[s][l][2] + curr_offset.2);
            beacons.insert(p);
        }
    }

    println!("num scanners: {}, num unique beams: {}", scanner_charts.len(), beacons.len());

}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let mut scanner_charts: Vec<Vec<Vec<i32>>> = Vec::new();
    let mut chars: Vec<Vec<i32>> = Vec::new();
    for i in 0..lines.len()  {
        if lines[i].len() == 0  {
            scanner_charts.push(chars.clone());
            continue;
        }

        let ch0 = lines[i].chars().nth(0).unwrap();
        let ch1 = lines[i].chars().nth(1).unwrap();
        if ch0 == '-' && ch1 == '-'  {
            chars.clear();
        } else  {
            let coords: Vec<i32> = lines[i].split(",").map(|p| p.parse::<i32>().unwrap()).collect();
            chars.push(coords.clone());
        }
    }

    let mut scanner_info: collections::HashMap<usize, (i32, i32, i32)> = collections::HashMap::new();
    let mut worker: collections::VecDeque<usize> = collections::VecDeque::new();
    worker.push_back(0);

    scanner_info.entry(0).or_insert((0, 0, 0));

    while !worker.is_empty() || scanner_info.len() < scanner_charts.len() {
        let mut i: usize = 0;
        if worker.is_empty()  {
            for i in 0..scanner_charts.len()  {
                if !scanner_info.contains_key(&i) {
                    worker.push_back(i);
                }
            }
        }

        match worker.front_mut() {
            Some(v) =>  {
                i = *v;
            },
            None => {}
        }
        worker.pop_front();

        let curr_offset = match scanner_info.get(&i)  {
            Some(v) => v.clone(),
            None =>  {
                scanner_info.entry(i).or_insert((0, 0, 0));
                (0, 0, 0)
            },
        };

        for j in 0..scanner_charts.len() {
            if scanner_info.contains_key(&j) || j == i {
                continue;
            }

            let (found, offset, perm, dir) = find_overlaps(&scanner_charts[i], &scanner_charts[j]);
            if found {
                println!("offset between({}, {}) is ({}, {}, {}), with perm {:?} and dir {:?})"
                         , i, j, offset.0, offset.1, offset.2, perm, dir);
                for c in 0..scanner_charts[j].len() {
                    let new_x = scanner_charts[j][c][perm[0]] * dir[0];
                    let new_y = scanner_charts[j][c][perm[1]] * dir[1];
                    let new_z = scanner_charts[j][c][perm[2]] * dir[2];
                    scanner_charts[j][c][0] = new_x;
                    scanner_charts[j][c][1] = new_y;
                    scanner_charts[j][c][2] = new_z;
                }
                let new_offset_x = curr_offset.0 + offset.0;
                let new_offset_y = curr_offset.1 + offset.1;
                let new_offset_z = curr_offset.2 + offset.2;

                worker.push_back(j);
                scanner_info.entry(j).or_insert((new_offset_x, new_offset_y, new_offset_z));
            }
        }
    }

    let mut offsets: Vec<(i32, i32, i32)> = vec![(0, 0, 0); scanner_charts.len()];

    for s in 0..scanner_charts.len()  {
        let curr_offset = match scanner_info.get(&s)  {
            Some(v) => v.clone(),
            None =>  {panic!("")},
        };

        println!("{}, curr offset({}, {}, {})", s, curr_offset.0, curr_offset.1, curr_offset.2);
        offsets[s] = curr_offset;
    }

    let mut max_dist: i32 = 0;
    for i in 0..offsets.len()-1  {
        for j in i+1..offsets.len() {
            let offset1 = offsets[i];
            let offset2 = offsets[j];
            let mdist = (offset1.0 - offset2.0).abs() + (offset1.1 - offset2.1).abs() + (offset1.2 - offset2.2).abs();
            if mdist > max_dist {
                max_dist = mdist;
            }
        }
    }

    println!("num scanners: {}, max dist: {}", scanner_charts.len(), max_dist);
}


}
