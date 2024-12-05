
fn count_increases(values: &[u32]) -> u32 {
    match values.iter().map(|&v| (v, 0)).reduce(|(prev, count), (v, _)| (v, count + u32::from(v>prev))) {
        Some((_, count)) => count,
        _ => 0
    }
}

fn part1(values: &[u32]) {
    let count = count_increases(values);
    println!("{} times increased", count);
}

fn part2(values: &[u32]) {
    let mut it = values.iter();
    let count = match (it.next(), it.next()) {
        (Some(&first), Some(&second)) => {
            let mut prev = (0, first, second);
            let windows: Vec<u32> = it.map(|&v| { prev = (prev.1, prev.2, v); prev.0+prev.1+prev.2 }).collect();
            count_increases(&windows)
        },
        _ => 0,
    };
    println!("{} windows increased", count);
}

fn main() {
    let input = include_str!("input.txt");
    let values: Vec<u32> = input.lines().map(|l| l.parse().unwrap()).collect();

    part1(&values);
    part2(&values);
}
