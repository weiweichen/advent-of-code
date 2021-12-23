pub mod day23 {


fn to_string(hallway: &Vec<char>, rooms:& Vec<Vec<char>>) -> String {
    hallway.iter().collect::<String>() +
        &rooms[0].iter().collect::<String>()+
        &rooms[1].iter().collect::<String>()+
        &rooms[2].iter().collect::<String>()+
        &rooms[3].iter().collect::<String>()
}

// check the path from start to end is all empty (inclusively)
fn all_empty(hallway: &Vec<char>, start: usize, end: usize) -> bool  {
    if start <  end {
        for i in start+1..end+1 {
            if hallway[i] != 'E' {
                return false;
            }
        }
    } else  {
        for i in end..start {
            if hallway[i] != 'E' {
                return false;
            }
        }
    }
    true
}

const room_names: [char; 4] = ['A', 'B', 'C', 'D'];

fn can_move_to_dst_room(rooms: &Vec<Vec<char>>, dest_room: usize, amp: char) -> usize  {
    let mut first_non_empty = rooms[dest_room].len();

    for i in 0..rooms[dest_room].len()  {
       if rooms[dest_room][i] != 'E' && rooms[dest_room][i] != amp  {
           return rooms[dest_room].len();
       }

       if rooms[dest_room][i] == amp && i < first_non_empty  {
          first_non_empty = i;
       }
    }
    if first_non_empty == 0 {
        return rooms[dest_room].len();
    } else {
        return first_non_empty - 1;
    }
}

// return list of (Amphoid, ((src_type, src_pos), (dst_type, dst_pos)), num_steps)
fn get_next_moves(hallway: &Vec<char>, rooms: &Vec<Vec<char>>)->VecDeque<(char, ((char, usize), (char,usize)), i32)> {
    let mut result: VecDeque<(char, ((char, usize), (char,usize)), i32)> = VecDeque::new();
    // move out of the rooms
    for r in 0..4  {
        let mut first_non_empty = rooms[r].len();
        for i in 0..rooms[r].len() {
            if rooms[r][i] != 'E'  {
               first_non_empty = i;
                break;
            }
        }

        if first_non_empty >= rooms[r].len() { continue;}
        let mut skip: bool = rooms[r][first_non_empty] == room_names[r];
        for i in first_non_empty..rooms[r].len() {
           skip &= (rooms[r][i] == room_names[r]);
        }
        if skip  {
            continue;
        }

        let src = (room_names[r], first_non_empty);
        let extra: i32 = first_non_empty as i32 + 1;
        let amp = rooms[r][first_non_empty];

        //if rooms[r][0] != 'E' {
        //    if rooms[r][0] == room_names[r] && rooms[r][1] == room_names[r]  {
        //        continue;
        //    }
        //    src = (room_names[r], 0);
        //    extra = 1;
        //} else if rooms[r][1] != 'E'  {
        //    if rooms[r][1] == room_names[r]  {
        //        continue;
        //    }
        //    src = (room_names[r], 1);
        //    extra = 2;
        //    amp = rooms[r][1];
        //} else  {
        //    continue;
        //}

        let mut p: i32 = (r as i32) * 2 + 2 - 1; // search left
        while p >= 0 && hallway[p as usize] == 'E'  {
            if p != 2 && p!= 4 && p != 6 && p!= 8 {
                // can't stay in front of the room
                result.push_back((amp, (src, ('H', p as usize)), (((2 + r * 2) as i32 - p).abs() + extra).try_into().unwrap()));
            }
            p -= 1;
        }

        p = (r as i32) * 2 + 2 + 1;
        while p < hallway.len().try_into().unwrap() && hallway[p as usize] == 'E'  {
            if p != 2 && p!= 4 && p != 6 && p!= 8 {
                // can't stay in front of the room
                result.push_back((amp, (src, ('H', p as usize)), (((2 + r * 2) as i32 - p).abs() + extra).try_into().unwrap()));
            }
            p += 1;
        }

        // check if can just move to the destination
        let dest_room = (amp as u32 - 65) as usize;
        let dest_room_pos = can_move_to_dst_room(rooms, dest_room, amp);

        if dest_room_pos < rooms[dest_room].len() {
            if all_empty(hallway, r *2 + 2, dest_room * 2 + 2)  {
                result.push_back((amp, (src, (amp, dest_room_pos)), ( (r as i32 - dest_room as i32).abs() * 2 + extra + dest_room_pos as i32 + 1).try_into().unwrap()));
            }
        }
    }

    // move hallway
    for i in 0..hallway.len()  {
        let src = ('H', i);

        let mut room_pos: i32 = 0;
        let mut dst_room: usize = 0;
        let mut room_name: char = hallway[i];

        match hallway[i]  {
            'A' =>  {
                room_pos = 2;
                dst_room = 0;
            },
            'B' =>  {
                room_pos = 4;
                dst_room = 1;
            },
            'C' =>  {
                room_pos = 6;
                dst_room = 2;
            },
            'D' =>  {
                room_pos = 8;
                dst_room = 3;
            },
            _ =>  {continue},
        }


        let dest_room_pos = can_move_to_dst_room(rooms, dst_room, room_name);

        if dest_room_pos < rooms[dst_room].len() && all_empty(hallway, i, room_pos as usize)  {
            result.push_back((hallway[i], (src, (room_name, dest_room_pos)), ((room_pos - i as i32).abs() + dest_room_pos as i32 + 1).try_into().unwrap()));
        }

        //if rooms[dst_room][0] == 'E' && all_empty(hallway, i, room_pos as usize)  {
        //    if rooms[dst_room][1] == 'E' {
        //        result.push_back((hallway[i], (src, (room_name, 1)), ((room_pos - i as i32).abs() + 2).try_into().unwrap()));
        //    } else if rooms[dst_room][1] == room_name {
        //        result.push_back((hallway[i], (src, (room_name, 0)), ((room_pos - i as i32).abs() + 1).try_into().unwrap()));
        //    }
        //}
    }


    result
}

use std::collections::VecDeque;
use std::collections::HashMap;
use std::collections::BinaryHeap;

fn update(hallway: &mut Vec<char>, rooms: &mut Vec<Vec<char>>, src: &(char, usize), dst: &(char, usize), amp: char) {
    let (src_p, src_loc) = src;
    let (dst_p, dst_loc) = dst;

    if *src_p == 'H'  {
        hallway[*src_loc] = 'E';
    } else if *src_p == 'A'  {
        rooms[0][*src_loc] = 'E';
    } else if *src_p == 'B'  {
        rooms[1][*src_loc] = 'E';
    } else if *src_p == 'C'  {
        rooms[2][*src_loc] = 'E';
    } else if *src_p == 'D'  {
        rooms[3][*src_loc] = 'E';
    }

    if *dst_p == 'H'  {
        hallway[*dst_loc] = amp;
    } else if *dst_p == 'A'  {
        rooms[0][*dst_loc] = amp;
    } else if *dst_p == 'B'  {
        rooms[1][*dst_loc] = amp;
    } else if *dst_p == 'C'  {
        rooms[2][*dst_loc] = amp;
    } else if *dst_p == 'D'  {
        rooms[3][*dst_loc] = amp;
    }
}

fn solved(rooms: &Vec<Vec<char>>)->bool  {
   for r in 0..4 {
       let mut amp: char = 'E';
       if r == 0  {
           amp = 'A'
       } else if r == 1 {
           amp = 'B'
       } else if r == 2 {
           amp = 'C'
       } else if r == 3 {
           amp = 'D'
       }

       for p in 0..2 {
           if rooms[r][p] != amp  {
               return false;
           }
       }
   }
    true
}

const amp_costs: [i32;4] = [1, 10, 100, 1000];

fn cost(path : &VecDeque<(char, ((char, usize), (char,usize)), i32)>) -> i32  {
    let mut result: i32 = 0;
    for (amp, src_dest, steps) in path  {
        result += amp_costs[(*amp as u32 - 65) as usize] * steps;
    }
    result
}

pub fn part1_orig<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

//    #############
//    #...........#
//    ###B#C#B#D###
//      #A#D#C#A#
//      #########

    let mut hallway: Vec<char> = vec!['E';lines[1].len() - 2];
    let mut line_chars: Vec<Vec<char>> = lines.iter().map(|line|  {
       line.chars().collect()
    }).collect();

    let mut rooms: Vec<Vec<char>> = vec![
        vec![line_chars[2][3], line_chars[3][3]],
        vec![line_chars[2][5], line_chars[3][5]],
        vec![line_chars[2][7], line_chars[3][7]],
        vec![line_chars[2][9], line_chars[3][9]]];

    let mut frontiers: VecDeque<VecDeque<(char, ((char, usize), (char,usize)), i32)>> = VecDeque::new();
    frontiers.push_back(get_next_moves(&hallway, &rooms));

    let mut path : VecDeque<(char, ((char, usize), (char,usize)), i32)> = VecDeque::new();
    let mut min_path : VecDeque<(char, ((char, usize), (char,usize)), i32)> = VecDeque::new();

    let mut done: bool = false;
    let mut min_cost: i32 = std::i32::MAX;
    let mut state: HashMap<String, i32> = HashMap::new();


    // back tracking
    while(!done  && !frontiers.is_empty())  {
        match frontiers.front_mut()  {
            Some(v) =>  {
                match v.front_mut()  {
                    Some(p) =>  {
                        path.push_front(*p);
                        //println!("move {}, from ({}, {}) to ({}, {})", p.0, p.1.0.0, p.1.0.1, p.1.1.0, p.1.1.1);
                        update(&mut hallway, &mut rooms, &p.1.0, &p.1.1, p.0);
                        v.pop_front();
                    },
                    None => {},
                }
            },
            None => {},
        }

        done = solved(&rooms);
        if (!done)  {
            let curr_cost = cost(&path);
            let key = to_string(&hallway, &rooms);
            let entry = state.entry(key).or_insert(std::i32::MAX);

            if *entry < curr_cost  {
                // remove the newly inserted move and reset the state
                match path.front_mut()  {
                    Some(p) =>  {
                        update(&mut hallway, &mut rooms, &p.1.1, &p.1.0, p.0);
                        //println!("recover {}, from ({}, {}) to ({}, {})", p.0, p.1.1.0, p.1.1.1, p.1.0.0, p.1.0.1);

                        path.pop_front();
                    },
                    None => {},
                }
            } else {
                *entry = curr_cost;
                let next_frontier = get_next_moves(&hallway, &rooms);
                if !next_frontier.is_empty() {
                    frontiers.push_front(next_frontier);
                } else  {
                    // remove the newly inserted move and reset the state
                    match path.front_mut()  {
                        Some(p) =>  {
                            update(&mut hallway, &mut rooms, &p.1.1, &p.1.0, p.0);
                            //println!("recover {}, from ({}, {}) to ({}, {})", p.0, p.1.1.0, p.1.1.1, p.1.0.0, p.1.0.1);

                            path.pop_front();
                        },
                        None => {},
                    }
                }
            }

            let mut drop: i32 = 0;
            let mut keep_droping = true;
            while keep_droping {
                match frontiers.front_mut() {
                    Some(f) => {
                        if f.is_empty() {
                            drop += 1;
                            frontiers.pop_front();
                        } else {
                            keep_droping = false;
                        }
                    },
                    None => {keep_droping = false;},
                }
            }

            while drop > 0  {
                drop -= 1;
                match path.front_mut()  {
                    Some(p) =>  {
                        update(&mut hallway, &mut rooms, &p.1.1, &p.1.0, p.0);
                        //println!("recover {}, from ({}, {}) to ({}, {})", p.0, p.1.1.0, p.1.1.1, p.1.0.0, p.1.0.1);
                        path.pop_front();
                    },
                    None => {},
                }
            }
        } else  {
            // found one solution.
            // compute cost
            done = false;

            // clear current status, keep searching
            let c = cost(&path);
            println!("found one solution with cost  {}", c);
            if c < min_cost  {
                min_cost = c;
                min_path = path.clone();
            }
            let mut drop: i32 = 0;
            let mut keep_droping = true;
            while keep_droping {
                match frontiers.front_mut() {
                    Some(f) => {
                        if f.is_empty() {
                            drop += 1;
                            frontiers.pop_front();
                        } else {
                            keep_droping = false;
                        }
                    },
                    None => {keep_droping = false;},
                }
            }

            drop += 1;

            while drop > 0  {
                drop -= 1;
                match path.front_mut()  {
                    Some(p) =>  {
                        update(&mut hallway, &mut rooms, &p.1.1, &p.1.0, p.0);
                        //println!("recover {}, from ({}, {}) to ({}, {})", p.0, p.1.1.0, p.1.1.1, p.1.0.0, p.1.0.1);
                        path.pop_front();
                    },
                    None => {},
                }
            }
        }
    }
    println!("min_cost: {}", min_cost);

}

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

//    #############
//    #...........#
//    ###B#C#B#D###
//      #A#D#C#A#
//      #########

    let mut hallway: Vec<char> = vec!['E';lines[1].len() - 2];
    let mut line_chars: Vec<Vec<char>> = lines.iter().map(|line|  {
        line.chars().collect()
    }).collect();

    let mut rooms: Vec<Vec<char>> = vec![
        vec![line_chars[2][3], line_chars[3][3]],
        vec![line_chars[2][5], line_chars[3][5]],
        vec![line_chars[2][7], line_chars[3][7]],
        vec![line_chars[2][9], line_chars[3][9]]];

    let mut frontiers: VecDeque<VecDeque<(char, ((char, usize), (char,usize)), i32)>> = VecDeque::new();
    frontiers.push_back(get_next_moves(&hallway, &rooms));

    let mut path : VecDeque<(char, ((char, usize), (char,usize)), i32)> = VecDeque::new();
    let mut min_path : VecDeque<(char, ((char, usize), (char,usize)), i32)> = VecDeque::new();

    let mut done: bool = false;
    let mut min_cost: i32 = std::i32::MAX;
    let mut dist: HashMap<(Vec<char>, Vec<Vec<char>>), i32> = HashMap::new();
    let mut q = BinaryHeap::new();
    q.push((0, (hallway.clone(), rooms.clone())));

    // find shortest path
    while let Some((cost,m)) = q.pop() {
        if solved(&m.1) {
            min_cost = -cost;
            break;
        }

        if let Some(&c) = dist.get(&m) {
            if -cost > c { continue; }
        }

        let next_moves = get_next_moves(&m.0, &m.1);
        for nm in next_moves.iter() {
            let curr_cost = amp_costs[(nm.0 as u32 - 65) as usize] * nm.2;
            let next_cost = -cost + curr_cost;
            let mut next_hallway = m.0.clone();
            let mut next_rooms = m.1.clone();
            update(&mut next_hallway, &mut next_rooms, &nm.1.0, &nm.1.1, nm.0);
            let next_state = (next_hallway, next_rooms);

            let &c = dist.get(&next_state).unwrap_or(&1000000);
            if c > next_cost {
                dist.insert(next_state.clone(), next_cost);
                q.push((-next_cost,next_state));
            }
        }
    }

    println!("min_cost: {}", min_cost);
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let mut hallway: Vec<char> = vec!['E';lines[1].len() - 2];
    let mut line_chars: Vec<Vec<char>> = lines.iter().map(|line|  {
        line.chars().collect()
    }).collect();

    let mut rooms: Vec<Vec<char>> = vec![
        vec![line_chars[2][3], 'D', 'D', line_chars[3][3]],
        vec![line_chars[2][5], 'C', 'B', line_chars[3][5]],
        vec![line_chars[2][7], 'B', 'A', line_chars[3][7]],
        vec![line_chars[2][9], 'A', 'C', line_chars[3][9]]];

    let mut frontiers: VecDeque<VecDeque<(char, ((char, usize), (char,usize)), i32)>> = VecDeque::new();
    frontiers.push_back(get_next_moves(&hallway, &rooms));

    let mut path : VecDeque<(char, ((char, usize), (char,usize)), i32)> = VecDeque::new();
    let mut min_path : VecDeque<(char, ((char, usize), (char,usize)), i32)> = VecDeque::new();

    let mut done: bool = false;
    let mut min_cost: i32 = std::i32::MAX;
    let mut dist: HashMap<(Vec<char>, Vec<Vec<char>>), i32> = HashMap::new();
    let mut q = BinaryHeap::new();
    q.push((0, (hallway.clone(), rooms.clone())));

    // find shortest path
    while let Some((cost,m)) = q.pop() {
        if solved(&m.1) {
            min_cost = -cost;
            break;
        }

        if let Some(&c) = dist.get(&m) {
            if -cost > c { continue; }
        }

        let next_moves = get_next_moves(&m.0, &m.1);
        for nm in next_moves.iter() {
            let curr_cost = amp_costs[(nm.0 as u32 - 65) as usize] * nm.2;
            let next_cost = -cost + curr_cost;
            let mut next_hallway = m.0.clone();
            let mut next_rooms = m.1.clone();
            update(&mut next_hallway, &mut next_rooms, &nm.1.0, &nm.1.1, nm.0);
            let next_state = (next_hallway, next_rooms);

            let &c = dist.get(&next_state).unwrap_or(&1000000);
            if c > next_cost {
                dist.insert(next_state.clone(), next_cost);
                q.push((-next_cost,next_state));
            }
        }
    }

    println!("min_cost: {}", min_cost);
}

}
