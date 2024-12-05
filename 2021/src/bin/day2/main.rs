use std::str::FromStr;

enum Command {
    Forward(u32),
    Down(u32),
    Up(u32),
}

impl FromStr for Command {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(" ").collect();
        let mag: u32 = parts[1].parse().unwrap();

        match parts[0] {
            "forward" => Ok(Command::Forward(mag)),
            "down" => Ok(Command::Down(mag)),
            "up" => Ok(Command::Up(mag)),
            _ => Err(format!("bad command: {}", s)),
        }
    }
}

fn part1(commands: &Vec<Command>) {
    let (depth, horiz) = commands.iter().fold((0, 0), |(depth, horiz), cmd| {
        match cmd {
            Command::Forward(v) => (depth, horiz + v),
            Command::Down(v) => (depth + v, horiz),
            Command::Up(v) => (depth - v, horiz),
        }
    });

    println!("depth={} horiz={} answer={}", depth, horiz, depth * horiz);
    assert_eq!(depth * horiz, 1383564);
}

fn part2(commands: &Vec<Command>) {
    let (depth, horiz, aim) = commands.iter().fold((0, 0, 0), |(depth, horiz, aim), cmd| {
        match cmd {
            Command::Forward(v) => (depth + aim * v, horiz + v, aim),
            Command::Down(v) => (depth, horiz, aim + v),
            Command::Up(v) => (depth, horiz, aim - v),
        }
    });
    println!("depth={} horiz={} aim={} answer={}", depth, horiz, aim, depth * horiz);
    assert_eq!(depth * horiz, 1488311643);
}

fn main() {
    let input = include_str!("input.txt");
    let commands: Vec<Command> = input
        .lines()
        .map(|s| s.parse().unwrap())
        .collect();

    part1(&commands);
    part2(&commands);
}
