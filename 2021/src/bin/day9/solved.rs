use std::cmp;
use num_traits::abs;

fn part1(positions: &[i32]) {
    let &min = positions.iter().reduce(cmp::min).unwrap();
    let &max = positions.iter().reduce(cmp::max).unwrap();

    let source: Vec<_> = (min..max+1).collect();
    let min_total: i32 = source.iter().map( |&dest| {
        positions.iter().map(|p| abs(p-dest)).sum()
    }).reduce(cmp::min).unwrap();

    println!("min total simple cost {}", min_total);
    
}

fn part2(positions: &[i32]) {
    let &min = positions.iter().reduce(cmp::min).unwrap();
    let &max = positions.iter().reduce(cmp::max).unwrap();

    let source: Vec<_> = (min..max+1).collect();
    let min_total: i32 = source.iter().map( |&dest| {
        positions.iter().map(|p| {
            let dist = abs(p-dest);
            dist * (dist+1) / 2
        }).sum()
    }).reduce(cmp::min).unwrap();

    println!("min total weighted cost {}", min_total);
}

fn main() {
    let input = include_str!("input.txt");
    let positions: Vec<i32> = input.split(",").map(|x| x.parse().unwrap()).collect();

    part1(&positions);
    part2(&positions);
}