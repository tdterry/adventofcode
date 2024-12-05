use std::collections::VecDeque;
use num_bigint::BigUint;
use num_traits::{One, Zero};

// array of [state] => count
struct State {
    state: VecDeque<BigUint>,
    generation: u32,
}

impl State {
    fn next(&mut self) {
        let v = &mut self.state;
        let first = v.pop_front().unwrap();
        v[6] += &first;
        v.push_back(first);
        self.generation += 1;
    }

    fn to_gen(&mut self, gen: u32) -> BigUint {
        for _ in self.generation..gen {
            self.next();
        }
        self.population()
    }

    fn population(&self) -> BigUint {
        self.state.iter().sum()
    }
}

impl FromIterator<usize> for State {
    fn from_iter<T: IntoIterator<Item=usize>>(iter: T) -> Self {
        let mut state: VecDeque<BigUint> = VecDeque::new();
        for _ in 0..9 {
            state.push_back(Zero::zero());
        }

        for c in iter {
            state[c] += &One::one();
        }

        State{state, generation: 0}
    }
}

fn part1(state: &mut State) {
    let count = state.to_gen(80);
    println!("number of fish after {} generations {}", state.generation, count);
    assert_eq!(count, 365862u64.into());

}

fn part2(state: &mut State) {
    let count = state.to_gen(256);
    println!("number of fish after {} generations {}", state.generation, count);
    assert_eq!(count, 1653250886439u64.into());
}

fn part3(state: &mut State) {
    let count = state.to_gen(1000);
    println!("number of fish after {} generations {}", state.generation, count);

    let count = state.to_gen(100000);
    println!("number of fish after {} generations {}", state.generation, count);
}

fn main() {
    let input = include_str!("input.txt");
    let mut state: State = input.split(",").map(|x| x.parse().unwrap()).collect();

    part1(&mut state);
    part2(&mut state);
    part3(&mut state);
}