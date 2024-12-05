use std::str::Lines;
use crate::ParseResult::Needs;

#[derive(Debug)]
enum ParseResult {
    Ok,
    Error(char),
    Needs(String),
}

fn score(c: char) -> u64 {
    match c {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => 0,
    }
}

fn parse_line(chars: &mut impl Iterator<Item=char>, expected: char, depth: usize) -> ParseResult
{
    loop {
        let next = chars.next();
        if next.is_none() {
            return if expected == ' ' {
                ParseResult::Ok
            } else {
                ParseResult::Needs(String::from(expected))
            };
        }

        let first = next.unwrap();
        // println!("{}{}", (0..depth).map(|_| " ").collect::<String>(), first);
        if first == expected {
            return ParseResult::Ok;
        }
        let parsed = match first {
            '(' => parse_line(chars, ')', depth + 1),
            '[' => parse_line(chars, ']', depth + 1),
            '{' => parse_line(chars, '}', depth + 1),
            '<' => parse_line(chars, '>', depth + 1),
            c => ParseResult::Error(c),
        };

        match parsed {
            ParseResult::Needs(s) => return Needs(s + &String::from(expected)),
            ParseResult::Error(e) => return ParseResult::Error(e),
            _ => (),
        }
    }
    // ParseResult::Ok
}

fn part1(lines: Lines) {
    let mut error_score = 0;
    for line in lines {
        let result = parse_line(&mut line.chars(), ' ', 0);
        if let ParseResult::Error(c) = result {
            println!("error {} {:?}", line, score(c));
            error_score += score(c);
        }
    }
    println!("error score {}", error_score);
}

fn part2(lines: Lines) {
    let mut scores: Vec<u64> = vec![];
    for line in lines {
        let mut incomplete_score = 0u64;
        let result = parse_line(&mut line.chars(), ' ', 0);
        if let ParseResult::Needs(chars) = &result {
            for c in chars.chars() {
                incomplete_score = match c {
                    ')' =>  incomplete_score * 5 + 1,
                    ']' =>  incomplete_score * 5 + 2,
                    '}' =>  incomplete_score * 5 + 3,
                    '>' =>  incomplete_score * 5 + 4,
                    _ => incomplete_score,
                };
            }
            scores.push(incomplete_score);
            println!("incomplete score {} {:?} {}", line, result, incomplete_score);
        }
    }
    scores.sort();
    println!("middle incomplete {}", scores[scores.len()/2]);
}

fn main() {
    let input = include_str!("input.txt");

    part1(input.lines());
    part2(input.lines());
}