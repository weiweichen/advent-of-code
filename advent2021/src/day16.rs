pub mod day16 {

static mut version_total: u64 = 0;

fn parse_literal(binary: &Vec<u8>, offset: usize) -> (u64, usize)  {
    //println!("parse_literal, offset: {}", offset);

    let mut consumed_bits = 0;
    let mut curr_offset = offset;
    let mut literal_value: u64 = 0;
    while true  {
        let leading_bit = binary[curr_offset];
        //println!("leading bit: {} ", leading_bit);
        curr_offset += 1;
        literal_value *= 16;
        literal_value += (binary[curr_offset] * 8 + binary[curr_offset + 1] * 4
            + binary[curr_offset + 2] * 2 + binary[curr_offset + 3]) as u64;
        curr_offset += 4;
        if leading_bit == 0  {
            break;
        }
    }
    // println!("literal value: {}, new_offset: {}, consumed: {}", literal_value, curr_offset, curr_offset - offset);
    (literal_value, curr_offset)
}

fn parse_operator(binary: &Vec<u8>, offset: usize, typeid: &i32) -> (u64, usize)  {
    //println!("parse_operator, offset: {}", offset);

    let result: Vec<u64> = Vec::new();
    let mut new_offset = offset;
    let length_type_id = binary[new_offset];
    new_offset += 1;

    let mut sub_packet_value: Vec<u64> = Vec::new();

    if length_type_id == 0  {
        // 15 bits containing length of sub-packets in bits
        let mut spl: usize = 0;
        for i in 0..15 {
            spl = spl * 2 + binary[new_offset] as usize;
            new_offset += 1;
        }
        // println!("length of sub-packets {}", spl);

        while spl > 0 {
            let result = parse_version(binary, new_offset);
            let consumed = result.1 - new_offset;
            spl -= consumed;
            new_offset = result.1;
            sub_packet_value.push(result.2);
        }

    } else  {
        // 11 bits for number of subpackets
        let mut num_sp: usize = 0;
        for i in 0..11 {
            num_sp = num_sp * 2 + binary[new_offset] as usize;
            new_offset += 1;
        }
        // println!("number sub-packets {}", num_sp);
        for p in 0..num_sp {
            let result = parse_version(binary, new_offset);
            new_offset = result.1;
            sub_packet_value.push(result.2);
        }
    }

    let mut final_result: u64 = 0;

    println!("====================================");
    if (*typeid == 0)  {
        // sum
        final_result = sub_packet_value.iter().sum();
        print!("sum ");
    } else if (*typeid == 1) {
        // product
        final_result = sub_packet_value.iter().product();
        print!("product ");
    } else if (*typeid == 2) {
        // min
        match sub_packet_value.iter().min()  {
            Some(v) => final_result = *v,
            None =>  {}
        }
        print!("min ");
    } else if (*typeid == 3) {
        // max
        match sub_packet_value.iter().max()  {
            Some(v) => final_result = *v,
            None =>  {}
        }
        print!("max ");
    } else if (*typeid == 5) {
        // greater than
        if sub_packet_value[0] > sub_packet_value[1]  {
            final_result = 1;
        }

        print!("greater than ");
    } else if (*typeid == 6) {
        // less than
        if sub_packet_value[0] < sub_packet_value[1]  {
            final_result = 1;
        }
        print!("less than ");
    } else if (*typeid == 7) {
        // equals
        if sub_packet_value[0] == sub_packet_value[1]  {
            final_result = 1;
        }
        print!("equals ");
    }

    println!("sub_packet_value {:?} = {}", sub_packet_value, final_result);
    (final_result, new_offset)
}

fn parse_typeid(binary: &Vec<u8>, offset: usize) -> (i32, usize, u64)  {
    let typeid : i32 = (binary[offset] * 4 + binary[offset + 1] * 2 + binary[offset + 2]).into();
    // println!("typeid {}", typeid);
    let mut new_offset = offset;
    let mut value: u64 = 0;
    if typeid == 4  {
        let result = parse_literal(binary, offset + 3);
        new_offset = result.1;
        value = result.0;
    } else  {
        let result = parse_operator(binary, offset + 3, &typeid);
        new_offset = result.1;
        value = result.0;
    }

    (typeid, new_offset, value)
}

fn parse_version(binary:&Vec<u8>, offset: usize) -> (i32, usize, u64)  {
    let version: i32 = (binary[offset] * 4 + binary[offset + 1] * 2 + binary[offset + 2]).into();
    //println!("====================================");
    //println!("version {}", version);
    unsafe {
        version_total += version as u64;
    }

    let (typeid, new_offset, value): (i32, usize, u64) = parse_typeid(binary, offset + 3);
    (version, new_offset, value)
}

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let binary: Vec<u8> = lines[0].chars().collect::<Vec<char>>().iter().flat_map(|c| {
        match c  {
            '0' => vec![0, 0, 0, 0],
            '1' => vec![0, 0, 0, 1],
            '2' => vec![0, 0, 1, 0],
            '3' => vec![0, 0, 1, 1],
            '4' => vec![0, 1, 0, 0],
            '5' => vec![0, 1, 0, 1],
            '6' => vec![0, 1, 1, 0],
            '7' => vec![0, 1, 1, 1],
            '8' => vec![1, 0, 0, 0],
            '9' => vec![1, 0, 0, 1],
            'A' => vec![1, 0, 1, 0],
            'B' => vec![1, 0, 1, 1],
            'C' => vec![1, 1, 0, 0],
            'D' => vec![1, 1, 0, 1],
            'E' => vec![1, 1, 1, 0],
            'F' => vec![1, 1, 1, 1],
            _ =>  vec![],
        }
    }).collect();
    println!("binary length: {}", binary.len());
    println!("binary: {:?}", binary);
    parse_version(&binary, 0);
    unsafe {
        println!("version total {}", version_total);
    }
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let binary: Vec<u8> = lines[0].chars().collect::<Vec<char>>().iter().flat_map(|c| {
        match c  {
            '0' => vec![0, 0, 0, 0],
            '1' => vec![0, 0, 0, 1],
            '2' => vec![0, 0, 1, 0],
            '3' => vec![0, 0, 1, 1],
            '4' => vec![0, 1, 0, 0],
            '5' => vec![0, 1, 0, 1],
            '6' => vec![0, 1, 1, 0],
            '7' => vec![0, 1, 1, 1],
            '8' => vec![1, 0, 0, 0],
            '9' => vec![1, 0, 0, 1],
            'A' => vec![1, 0, 1, 0],
            'B' => vec![1, 0, 1, 1],
            'C' => vec![1, 1, 0, 0],
            'D' => vec![1, 1, 0, 1],
            'E' => vec![1, 1, 1, 0],
            'F' => vec![1, 1, 1, 1],
            _ =>  vec![],
        }
    }).collect();
    println!("binary length: {}", binary.len());
    println!("binary: {:?}", binary);
    let result = parse_version(&binary, 0);
    unsafe {
        println!("version total {}, final value {}", version_total, result.2);
    }
}

}
