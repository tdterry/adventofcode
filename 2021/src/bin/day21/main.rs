use std::cmp::max;
use std::collections::HashMap;
use itertools::{Itertools, MinMaxResult};

use regex::Regex;

fn main() {
    // let input = include_str!("sample_input.txt");
    let input = include_str!("input.txt");
    let lines = input.lines();

    let regex = Regex::new(r"(\d+)$").unwrap();
    let player_positions: Vec<usize> = lines.map(|l| regex.captures(l).unwrap()[1].parse().unwrap()).collect();
    let player1_space: usize = player_positions[0];
    let player2_space: usize = player_positions[1];
    dbg!(player1_space);
    dbg!(player2_space);

    part1(player1_space, player2_space);
    part2(player1_space, player2_space);
}

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
struct GameState {
    positions: [usize; 2],
    scores: [usize; 2],
    turn: usize,
    roll_num: usize,
}

impl GameState {
    fn new(player1_space: usize, player2_space: usize) -> Self {
        Self {
            positions: [player1_space, player2_space],
            scores: [0, 0],
            turn: 0,
            roll_num: 0,
        }
    }

    fn apply_roll(&self, roll: usize) -> Self {
        let Self { mut positions, mut scores, mut turn, mut roll_num } = self;

        positions[turn] = (positions[turn] + roll + 9) % 10 + 1;
        roll_num += 1;

        if roll_num == 3 {
            scores[turn] += positions[turn];
            turn = 1 - turn;
            roll_num = 0;
        }

        Self {
            positions,
            scores,
            turn,
            roll_num,
        }
    }
}

fn part1(player1_space: usize, player2_space: usize) {
    let mut game: GameState = GameState::new(player1_space, player2_space);

    let mut die = 100;
    let mut num_rolls = 0;
    let loser_points = loop {
        match game.scores.iter().minmax() {
            MinMaxResult::MinMax(&min, &max) if max >= 1000 && game.roll_num == 0 => break min,
            _ => {
                die = (die % 100) + 1;
                num_rolls += 1;
                // die = die.roll();
                game = game.apply_roll(die);
            }
        };
    };

    println!("answer {}", loser_points * num_rolls);
    assert_eq!(loser_points * num_rolls, 1196172);
}

fn part2(player1_space: usize, player2_space: usize) {
    let mut cache = HashMap::new();
    let game: GameState = GameState::new(player1_space, player2_space);

    let wins = quantum_play(&mut cache, &game);
    println!("universe wins {:?}", wins);
    println!("most wins {}", max(wins.0, wins.1));
    assert_eq!(max(wins.0, wins.1), 106768284484217);
}

// return (player1 universes, player2 universes)
type GameCache = HashMap<GameState, (u64, u64)>;

fn quantum_play(cache: &mut GameCache, game_state: &GameState) -> (u64, u64) {
    match cache.get(game_state) {
        Some(&solution) => solution,
        None => {
            let ret = match game_state.scores.iter().minmax() {
                MinMaxResult::MinMax(_, &max) if max >= 21 && game_state.roll_num == 0 =>
                    (u64::from(game_state.scores[0] == max), u64::from(game_state.scores[1] == max)),
                _ => (1..4)
                    .map(|r| quantum_play(cache, &game_state.apply_roll(r)))
                    .fold((0, 0), |acc, x| (acc.0 + x.0, acc.1 + x.1))
            };

            cache.insert(*game_state, ret);
            ret
        }
    }
}