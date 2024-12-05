fn part1(commands: &[String]) {
    let mut count_ones = [0; 12];
    for c in commands {
        let chars: Vec<char> = c.chars().collect();
        for (i, &c) in chars.iter().enumerate() {
            if c == '1' {
                count_ones[i] += 1
            }
        }
    }
    let total = commands.len();
    println!("count_ones={:?}", count_ones);

    let gamma: Vec<bool> = count_ones.iter().map(|&c| c * 2 >= total).collect();
    println!("gamma={:?}", gamma);
    println!("{}", 2663 * 1432);
}

fn part2(commands: &[String]) {
    let mut potential_max: Vec<String> = commands.to_vec();
    let mut potential_min: Vec<String> = commands.to_vec();
    for i in 0..12 {

        if potential_max.len() > 1 {
            let mut count = 0;
            for c in potential_max.clone().into_iter() {
                let chars: Vec<char> = c.chars().collect();
                if chars[i] == '1' {
                    count += 1
                }
            }
            println!("i={} count={}", i, count);
            if count * 2 >= potential_max.len() {
                // filter 1's
                potential_max = potential_max.into_iter().filter(|c| c.chars().collect::<Vec<char>>()[i] == '1').collect();
            } else {
                // filter 0's
                potential_max = potential_max.into_iter().filter(|c| c.chars().collect::<Vec<char>>()[i] == '0').collect();
            }
        }

        if potential_min.len() > 1 {
            let mut count = 0;
            for c in potential_min.clone().into_iter() {
                let chars: Vec<char> = c.chars().collect();
                if chars[i] == '1' {
                    count += 1
                }
            }
            println!("i={} count={}", i, count);
            if count * 2 >= potential_min.len() {
                // filter 1's
                potential_min = potential_min.into_iter().filter(|c| c.chars().collect::<Vec<char>>()[i] == '0').collect();
            } else {
                // filter 0's
                potential_min = potential_min.into_iter().filter(|c| c.chars().collect::<Vec<char>>()[i] == '1').collect();
            }
        }
    }

    println!("potential_max={:?}", potential_max);
    println!("potential_min={:?}", potential_min);
    println!("{}", 2526 * 1184);
}

fn main() {
    let input = include_str!("input.txt");
    let commands: Vec<String> = input
        .lines()
        .map(|s| s.parse().unwrap())
        .collect();

    // println!("{:?}", commands);

    part1(&commands);
    part2(&commands);
}
