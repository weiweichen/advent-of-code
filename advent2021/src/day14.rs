pub mod day14 {

pub fn part1<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let mut pair_rules: std::collections::HashMap<String, char> = std::collections::HashMap::new();
    lines.iter().rev().take(lines.len() - 2).rev().for_each(|line| {
        let s: Vec<&str> = line.split(" -> ").collect();
        let c = s[1].chars().collect::<Vec<char>>()[0];
        pair_rules.entry(s[0].to_string()).or_insert(c);
    });

    let mut template: Vec<char> = lines[0].chars().collect();
    let step:i32 = 10;

    for s in 0..step  {
        let mut curr_template: Vec<char> = Vec::new();
        for i in 0..template.len() - 1  {
            let pair_str: String = vec![template[i], template[i + 1]].iter().collect();
            curr_template.push(template[i]);
            match pair_rules.get(&pair_str)  {
                Some(c) =>  {
                    curr_template.push(*c);
                },
                None => {},
            };
            if i == template.len() - 2 {curr_template.push(template[i + 1]);}
        }
        template = curr_template;
        // println!("{:?}", template);
    }

    let mut min: u64 = u64::MAX;
    let mut max: u64 = 0;

    let mut histogram: std::collections::HashMap<char, u64> = std::collections::HashMap::new();
    template.iter().for_each(|c| {
        let freq = histogram.entry(*c).or_insert(0);
        *freq += 1;
    });

    for (c, freq) in histogram.iter()  {
        if *freq < min  {
            min = *freq;
        }
        if *freq > max  {
            max = *freq;
        }
    }

    println!("min: {}, max: {}, max - min: {}", min, max, max - min);
}

pub fn part2<R: std::io::BufRead>(reader: R) {
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let template: Vec<char> = lines[0].chars().collect();
    println!("{:?}", template);

    let mut pair_rules: std::collections::HashMap<(char, char), char> = std::collections::HashMap::new();

    lines.iter().rev().take(lines.len() - 2).rev().for_each(|line| {
        let s: Vec<&str> = line.split(" -> ").collect();
        let c = s[1].chars().collect::<Vec<char>>()[0];
        let chars :Vec<char> = s[0].chars().collect();
        pair_rules.entry((chars[0], chars[1])).or_insert(c);
    });

    let mut pair_frequencies: std::collections::HashMap<(char, char), u64> = std::collections::HashMap::new();
    let step:i32 = 40;
    // NNCB
    // NN -> 1 => C
    // NC -> 1 => B
    // CB -> 1 => H
    // NC -> 1
    // CN -> 1
    // NB -> 1
    // BC -> 1
    // CH -> 1
    // HB -> 1

    for i in 0..template.len() - 1  {
        let pair = (template[i], template[i+1]);
        let freq = pair_frequencies.entry(pair).or_insert(0);
        *freq += 1;
    }
    //println!("============= step {} ==============", 0);
    //for ((c1, c2), freq) in pair_frequencies.iter()  {
    //    println!("({}, {}) => {}", c1, c2, freq);
    //}

    for s in 0..step  {
        let mut curr_pair_frequencies: std::collections::HashMap<(char, char), u64> = std::collections::HashMap::new();
        for ((c1, c2), freq) in pair_frequencies  {
            match pair_rules.get(&(c1, c2))  {
                Some(c) => {
                    let pair1 = (c1,*c);
                    let pair2 = (*c, c2);
                    let freq1 = curr_pair_frequencies.entry(pair1).or_insert(0);
                    *freq1 += freq;
                    let freq2 = curr_pair_frequencies.entry(pair2).or_insert(0);
                    *freq2 += freq;
                }
                None => {
                    let curr_freq = curr_pair_frequencies.entry((c1, c2)).or_insert(0);
                    *curr_freq += freq;
                }
            };
        };
        pair_frequencies = curr_pair_frequencies;
        //println!("============= step {} ==============", s + 1);
        //for ((c1, c2), freq) in pair_frequencies.iter()  {
        //    println!("({}, {}) => {}", c1, c2, freq);
        //}
    }

    let mut min: u64 = u64::MAX;
    let mut max: u64 = 0;

    let mut histogram: std::collections::HashMap<char, u64> = std::collections::HashMap::new();

    for ((c1, c2), freq) in pair_frequencies {
        let curr_freq = histogram.entry(c1).or_insert(0);
        *curr_freq += freq;
    };

    let freq = histogram.entry(template[template.len()-1]).or_insert(0);
    *freq += 1;

    for (c, freq) in histogram.iter()  {
        if *freq < min  {
            min = *freq;
        }
        if *freq > max  {
            max = *freq;
        }
    }

    println!("min: {}, max: {}, max - min: {}", min, max, max - min);
}

}
