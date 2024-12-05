use std::ops::Index;
use itertools::Itertools;

fn is_digit(DIGITS: &[u8], wiring: &str, pattern: &str) -> bool {
    let mut digits: u8 = 0;
    pattern.chars().for_each(|c| {
        let pos = wiring.find(c).unwrap();
        let position = 6 - pos;
        digits |= 1u8 << position;
    });
    DIGITS.contains(&digits)
}

#[derive(Debug)]
struct Board<'a> {
    digits: Vec<&'a str>,
    output: Vec<&'a str>,
}

fn part1(lines: &[Board]) {
    let mut count = 0;
    for b in lines {
        println!("{:?}", b);
        count += b.output.iter().filter(|x| x.len() == 2 || x.len() == 4 || x.len() == 3 || x.len() == 7).count()
    }
    println!("count {}", count);
}

fn part2(DIGITS: &[u8], boards: &[Board]) {
    let mut total = 0;
    for board in boards {
        for wiring in "abcdefg".chars().permutations(7) {
            let wiring = wiring.iter().collect::<String>();
            let solved = board.digits.iter().all(|&d| is_digit(DIGITS, &wiring, d));
            if solved {
                let mut num = 0;
                for &d in &board.output {
                    let mut digit = 0u8;
                    d.chars().for_each(|c| digit |= 1u8 << 6-wiring.find(c).unwrap());
                    let digit = DIGITS.iter().find_position(|&&v| v==digit).unwrap().0;
                    // println!("{} {}", d, digit);
                    num = num * 10 + digit;
                }
                // println!("{}", num);
                total += num;
                break;
            }

        }
    }
    println!("total {}", total);
}

fn main() {
    let DIGITS: Vec<u8> = vec![
        0b01110111, // 0
        0b00010010, // 1
        0b01011101, // 2
        0b01011011, // 3
        0b00111010, // 4,
        0b01101011, // 5
        0b01101111, // 6
        0b01010010, // 7
        0b01111111, // 8
        0b01111011, // 9
    ];
    let input = include_str!("input.txt");

    let boards: Vec<Board> = input.lines().map(
        |x| -> Board {
            let parts: Vec<Vec<&str>> = x.split(" | ").map(
                |x| x.split(" ").collect()
            ).collect();
            Board {
                digits: parts[0].clone(),
                output: parts[1].clone(),
            }
        }
    ).collect();

    part1(&boards);
    part2(&DIGITS, &boards);
}