use std::fmt;
use std::fmt::{Debug, Formatter};
use std::str::{FromStr, Lines};
use regex::Regex;

struct ImmutableEntry(u32, bool);

impl ImmutableEntry {
    fn marked(self) -> Self {
        Self(self.0, true)
    }
}

#[derive(Debug,Clone,Copy)]
struct Entry(u32, bool);

impl Entry {
    fn mark_if_eq(&mut self, v: u32) {
        if self.0 == v {
            self.1 = true;
        }
    }

    fn new() -> Self {
        Self(0, false)
    }
}

impl FromStr for Entry {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.parse().unwrap(), false))
    }
}

struct Board {
    entries: [[Entry; 5]; 5],
    turns: usize,
    score: u32,
}

impl Board {
    fn mark(&mut self, s: u32) {
        for y in 0..5 {
            for x in 0..5 {
                self.entries[y][x].mark_if_eq(s);
            }
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

    // play until the board wins, save turns and score
    fn play(&mut self, draws: &[u32]) {
        self.turns = 0;
        self.score = 0;

        for &draw in draws.iter() {
            self.turns += 1;
            self.mark(draw);
            if self.wins() {
                self.score = self.entries.iter().flatten().filter(|e| !e.1).map(|e| e.0).sum();
                // self.score = self.entries.iter().map(|r| {
                //     r.iter().filter(|e| !e.1).map(|e| e.0).sum::<u32>()
                // }).sum();
                self.score *= draw;
                return ()
            }
        }
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

impl From<&mut Lines<'_>> for Board {
    fn from(lines: &mut Lines<'_>) -> Self {
        let mut entries = [[Entry::new(); 5]; 5];
        let splitter = Regex::new(r#"\s+"#).unwrap();
        for y in 0..5 {
            splitter.split(lines.next().unwrap().trim()).map(|s| -> String { s.trim().parse().unwrap() }).map(|e| e.parse().unwrap()).enumerate().for_each(|(i, e)| entries[y][i] = e);
        }
        Board{
            entries,
            turns: 0,
            score: 0,
        }
    }
}

fn main() {
    let input = include_str!("input.txt");
    let mut lines = input.lines();
    let draws: Vec<u32> = lines.next().unwrap().split(",").map(|n| n.parse().unwrap()).collect();

    let mut boards: Vec<Board> = vec![];

    println!("numbers={:?}", draws);
    while let Some(_) = lines.next() {
        let mut board = Board::from(&mut lines);
        board.play(&draws);
        boards.push(board);
    }

    let first_winner = boards.iter().fold((draws.len(), 0), |acc, x| if x.turns < acc.0 { (x.turns, x.score)} else {acc});
    println!("winning score {}", first_winner.1);
    assert_eq!(first_winner.1, 63424);

    let last_winner = boards.iter().fold((0, 0), |acc, x| if x.turns > acc.0 { (x.turns, x.score)} else {acc});
    println!("last winning score {}", last_winner.1);
    assert_eq!(last_winner.1, 23541);
}