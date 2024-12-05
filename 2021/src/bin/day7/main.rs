use std::cmp;
use num_traits::abs;

fn minmax<T: Ord + Copy>(acc: (T, T), x: T) -> (T, T) {
    (cmp::min(acc.0, x), cmp::max(acc.1, x))
}

fn calc_cost<F>(positions: &[i32], min: i32, max: i32, weight_fn: F) -> i32
    where F: Fn(i32, i32) -> i32
{
    (min..max + 1).map(|dest| {
        positions.iter().map(|&x| weight_fn(dest, x)).sum()
    }).reduce(cmp::min).unwrap()
}

fn part1(positions: &[i32]) {
    let (min, max) = positions.iter().fold((i32::MAX, i32::MIN), |acc, &x| minmax(acc, x));

    let weight_fn = |a, b| abs(a - b);
    let cost = calc_cost(positions, min, max, weight_fn);

    println!("min total simple cost {}", cost);
    assert_eq!(cost, 352331);
}

fn part2(positions: &[i32]) {
    let (min, max) = positions.iter().fold((i32::MAX, i32::MIN), |acc, &x| minmax(acc, x));

    let weight_fn = |a, b| {
        let dist = abs(a - b);
        dist * (dist + 1) / 2
    };
    let cost = calc_cost(positions, min, max, weight_fn);

    println!("min total weighted cost {}", cost);
    assert_eq!(cost, 99266250);
}

fn main() {
    let input = include_str!("input.txt");
    let positions: Vec<i32> = input.split(",").map(|x| x.parse().unwrap()).collect();

    part1(&positions);
    part2(&positions);
}