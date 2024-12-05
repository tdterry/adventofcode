use std::collections::HashMap;

fn main() {
    // let input = include_str!("sample_input.txt");
    let input = include_str!("input.txt");
    let lines = input.lines();

    let edges: Vec<(String, String)> = lines.map(|l| {
        println!("{}", l);
        let mut it = l.split("-");
        (it.next().unwrap().into(), it.next().unwrap().into())
    }).collect();

    println!();

    part1(&edges);
    part2(&edges);
}

fn part1(edges: &Vec<(String, String)>) {
    // dbg!(edges);

    let count = walk(edges, &Vec::new(), "start".into(), "end".into());
    println!("count {}", count);
}

fn part2(edges: &Vec<(String, String)>) {
    // dbg!(edges);

    let count = walk2(edges, &Vec::new(), "start".into(), "end".into());
    println!("count {}", count);
}

fn walk(edges: &Vec<(String, String)>, path: &Vec<String>, node: &str, end: &str) -> u64 {
    let mut path = path.clone();
    path.push(node.into());

    if node == end {
        // println!("{:?}", path);
        return 1;
    }

    let next: Vec<String> = edges
        .iter()
        .map(|(from, to)| match node {
            s if s == from => to,
            s if s == to => from,
            _ => "",
        }.into())
        .filter(|x: &String| x != "" && (
            x.chars().next().unwrap().is_uppercase()
                || !path.contains(x)
        ))
        .collect();

    next.iter().map(|n| {
        walk(edges, &path, n, end)
    }).sum()
}

fn walk2(edges: &Vec<(String, String)>, path: &Vec<String>, node: &str, end: &str) -> u64 {
    let mut path = path.clone();
    path.push(node.into());

    if node == end {
        // println!("{:?}", path);
        return 1;
    }

    let next: Vec<String> = edges
        .iter()
        .map(|(from, to)| match node {
            s if s == from => to,
            s if s == to => from,
            _ => "",
        }.into())
        .filter(|x: &String| {
            // println!("at {:?} trying {}", path, x);
            x != "" && x != "start" && (
                x.chars().next().unwrap().is_uppercase()
                    || at_most_one_repeat(&path, x)
            )
        })
        .collect();

    next.iter().map(|n| {
        walk2(edges, &path, n, end)
    }).sum()
}

fn at_most_one_repeat(path: &Vec<String>, next: &str) -> bool {
    let mut counts: HashMap<String, usize> = HashMap::new();

    for p in path {
        if p.chars().next().unwrap().is_lowercase() {
            counts.insert(p.clone(), counts.get(p).or_else(|| Some(&0)).unwrap() + 1);
        }
    }

    counts.insert(next.into(), counts.get(next).or_else(|| Some(&0)).unwrap() +1);

    let mut repeats = 0;
    for (_, &v) in counts.iter() {
        if v > 2 {
            return false;
        }
        if v > 1 {
            repeats += 1;
        }
    }

    repeats <= 1
}