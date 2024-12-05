use std::cmp::max;
use std::collections::HashMap;

fn expand_counts<'a>(
    cache: &'a mut HashMap<(char, char, usize), HashMap<char, u64>>,
    rules: &HashMap<(char, char), char>,
    pair: &(char, char),
    step: usize,
) -> &'a HashMap<char, u64> {
    let &(a, b) = pair;
    if cache.get(&(a, b, step)).is_some() {
        return cache.get(&(a, b, step)).unwrap();
    }

    let mut ret: HashMap<char, u64> = HashMap::new();
    let &replacement = rules.get(pair).unwrap();
    ret.insert(replacement, 1);
    if step > 1 {
        {
            let left = expand_counts(cache, rules, &(pair.0, replacement), step - 1);
            for (c, &count) in left.iter() {
                ret.insert(*c, ret.get(c).or_else(|| Some(&0)).unwrap() + count);
            }
        }

        {
            let right = expand_counts(cache, rules, &(replacement, pair.1), step - 1);
            for (c, &count) in right.iter() {
                ret.insert(*c, ret.get(c).or_else(|| Some(&0)).unwrap() + count);
            }
        }
    }

    cache.insert((a, b, step), ret);
    cache.get(&(a, b, step)).unwrap()
}

fn main() {
    let input = include_str!("input.txt");
    let mut lines = input.lines();
    let mut seq: String = lines.next().unwrap().into();
    lines.next();

    let mut rules: HashMap<(char, char), char> = HashMap::new();
    while let Some(line) = lines.next() {
        dbg!(line);
        let rule: Vec<_> = line.split(" -> ").collect();
        let mut chars = rule[0].chars();
        rules.insert((chars.next().unwrap(), chars.next().unwrap()), rule[1].chars().next().unwrap());
    }

    let steps = 40;
    println!("Template: {}", seq);
    let mut counts: HashMap<char, u64> = HashMap::new();
    for c in seq.chars() {
        counts.insert(c, *counts.get(&c).or_else(|| Some(&0)).unwrap() + 1);
    }

    let mut chars = seq.chars();
    let mut pair = (' ', chars.next().unwrap());

    let mut cache: HashMap<(char, char, usize), HashMap<char, u64>> = HashMap::new();
    while let Some(c) = chars.next() {
        pair = (pair.1, c);
        dbg!(pair);
        let sub_counts = expand_counts(&mut cache, &rules, &pair, steps);
        for (&c, &count) in sub_counts.iter() {
            counts.insert(c, counts.get(&c).or_else(|| Some(&0)).unwrap() + count);
        }
    }
    dbg!(&counts);

    let mut c_most: char = ' ';
    let mut count_most = 0;
    let mut c_least = ' ';
    let mut count_least = u64::MAX;
    for (&c, &count) in counts.iter() {
        if count > count_most {
            count_most = count;
            c_most = c;
        }
        if count < count_least {
            count_least = count;
            c_least = c;
        }
    }

    println!("{}", count_most - count_least);
}