use std::fmt;
use std::fmt::{Debug, Formatter};
use std::str::{FromStr, Lines};
use regex::Regex;

#[derive(Debug)]
struct Entry(String, bool);

impl Entry {
    fn mark(&mut self) {
        self.1 = true;
    }
}

impl FromStr for Entry {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(String::from(s), false))
    }
}


struct Board {
    entries: Vec<Vec<Entry>>,
    play_count: usize,
    score: u32,
}

impl Board {
    fn new(entries: Vec<Vec<Entry>>) -> Self {
        Self { entries, play_count: 0, score: 0 }
    }

    fn mark(&mut self, s: &str) {
        for row in &mut self.entries {
            row.iter_mut().for_each(|e| if e.0 == s { e.mark() });
        }
    }

    fn at(&self, x: usize, y: usize) -> &Entry {
        &self.entries[y][x]
    }

    fn wins(&self) -> bool {
        // check rows
        for y in 0..5 {
            let mut win = true;
            for x in 0..5 {
                if !self.at(x, y).1 {
                    win = false;
                }
            }
            if win {
                return true;
            }
        }

        // check columns
        for x in 0..5 {
            let mut win = true;
            for y in 0..5 {
                if !self.at(x, y).1 {
                    win = false;
                }
            }
            if win {
                return true;
            }
        }

        // check x=y diag
        let mut win = true;
        for x in 0..5 {
            if !self.at(x, x).1 {
                win = false
            }
        }
        if win {
            return true;
        }

        // check x=5-y diag {
        let mut win = true;
        for x in 0..5 {
            if !self.at(x, 4 - x).1 {
                win = false
            }
        }
        if win {
            return true;
        }

        false
    }

    // play until the board wins, return the number of turns
    fn play(&mut self, draws: &[&str]) {
        let mut draw = "";
        let mut draws = draws.iter();
        let mut count = 0;
        while !self.wins() {
            count += 1;
            draw = draws.next().unwrap();
            self.mark(draw.as_ref());
        }
        self.play_count = count;

        self.score = 0;
        for x in 0..5 {
            for y in 0..5 {
                if !self.at(x, y).1 {
                    self.score += self.at(x,y).0.parse::<u32>().unwrap();
                }
            }
        }
        self.score *= draw.parse::<u32>().unwrap();
    }
}

impl Debug for Board {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Board(\n")?;
        for r in &self.entries {
            f.write_str("    ")?;
            fmt::Debug::fmt(&r, f)?;
            f.write_str(",\n")?;
        }
        f.write_str(")")?;
        Ok(())
    }
}

fn parse_board(lines: &mut Lines) -> Board
{
    let splitter = Regex::new(r#"\s+"#).unwrap();
    let mut table = Vec::<Vec<Entry>>::new();
    for _ in 0..5 {
        let row: Vec<Entry> = splitter.split(lines.next().unwrap().trim()).map(|s| -> String { s.trim().parse().unwrap() }).map(|e| e.parse().unwrap()).collect();
        table.push(row)
    }
    Board::new(table)
}

fn main() {
    let input = include_str!("input.txt");
    let mut lines = input.lines();
    let draws: Vec<&str> = lines.next().unwrap().split(",").collect();

    let mut boards: Vec<Board> = vec![];

    println!("numbers={:?}", draws);
    while let Some(_) = lines.next() {
        let mut board = parse_board(&mut lines);
        board.mark("24");
        println!("{:?}", board);

        board.play(&draws);
        println!("wins in {} with score {}", board.play_count, board.score);

        boards.push(board);
    }

    let winner = boards.iter().fold((draws.len(), 0), |acc, x| if x.play_count < acc.0 { (x.play_count, x.score)} else {acc});
    println!("winning score {}", winner.1);

    let last_winner = boards.iter().fold((0, 0), |acc, x| if x.play_count > acc.0 { (x.play_count, x.score)} else {acc});
    println!("last winning score {}", last_winner.1);
}