#![feature(box_syntax, box_patterns)]

use std::ops::DerefMut;
use std::str::Chars;
use itertools::max;
use regex::Regex;
use crate::SnailNum::{Pair, Regular};

#[derive(Debug, Clone)]
enum SnailNum {
    Regular(u32),
    Pair(Box<SnailNum>, Box<SnailNum>),
}

fn main() {
    let input = include_str!("input.txt");
    // let input = include_str!("input.txt");
    let mut lines = input.lines();

    let mut snail_nums: Vec<SnailNum> = Vec::new();
    for l in lines {
        snail_nums.push(parse_snail_num(&mut l.chars()));
    }

    let mut sn = snail_nums[0].clone();
    for s1 in &snail_nums[1..] {
        sn = add_snail_num(&sn, s1);
    }
    // dbg!(&sn);
    println!("magnitude {}", magnitude(&sn));

    let mut max_mag = 0;
    for i in 0..snail_nums.len() {
        for j in 0..snail_nums.len() {
            if i == j {
                continue;
            }
            let sn = add_snail_num(&snail_nums[i], &snail_nums[j]);
            let mag = magnitude(&sn);
            if mag > max_mag {
                max_mag = mag;
            }
        }
    }

    println!("max mag {}", max_mag);
}

fn parse_snail_num(chars: &mut Chars) -> SnailNum {
    let c = chars.next().unwrap();

    if c == '[' {
        let left = parse_snail_num(chars);
        chars.next(); // throw away ,
        let right = parse_snail_num(chars);
        chars.next(); // throw away ]
        SnailNum::Pair(Box::new(left), Box::new(right))
    } else {
        SnailNum::Regular(c.to_digit(10).unwrap())
    }
}

fn add_snail_num(s0: &SnailNum, s1: &SnailNum) -> SnailNum {
    let mut sn = SnailNum::Pair(Box::new(s0.clone()), Box::new(s1.clone()));
    reduce_snail_num(&mut sn);
    sn
}

fn reduce_snail_num(sn: &mut SnailNum) {
    let mut can_reduce = true;
    while can_reduce {
        can_reduce = explode(sn, &mut SnailNum::Regular(0), &mut SnailNum::Regular(0), 0) ||
            split(sn, 0);
    }
}

fn first(sn: &mut SnailNum) -> &mut SnailNum {
    match sn {
        SnailNum::Pair(x, y) => first(x),
        x => x,
    }
}

fn last(sn: &mut SnailNum) -> &mut SnailNum {
    match sn {
        SnailNum::Pair(x, y) => last(y),
        x => x,
    }
}

fn explode(sn: &mut SnailNum, left: &mut SnailNum, right: &mut SnailNum, depth: usize) -> bool {
    // println!("explode depth {} {:?} left={:?} right={:?}", depth, sn, left, right);
    let ret = if let SnailNum::Pair(box x, box y) = sn {
        if depth == 4 {
            if let SnailNum::Regular(l) = left {
                if let SnailNum::Regular(x) = x {
                    *l += *x;
                }
            }
            if let SnailNum::Regular(r) = right {
                if let SnailNum::Regular(y) = y {
                    *r += *y;
                }
            }
            *sn = SnailNum::Regular(0);
            true
        } else {
            explode(x, last(left), first(y), depth + 1) ||
                explode(y, last(x), first(right), depth + 1)
        }
    } else {
        false
    };
    // println!("explode depth {} {:?} left={:?} right={:?} -> {}", depth, sn, left, right, ret);
    ret
}

fn split(sn: &mut SnailNum, depth: usize) -> bool {
    // println!("split depth {} {:?}", depth, sn);
    let ret = match sn {
        SnailNum::Regular(x) if *x >= 10 => {
            let l = *x / 2;
            let r = (*x + 1) / 2;
            let r = (*x + 1) / 2;
            *sn = SnailNum::Pair(Box::new(SnailNum::Regular(l)), Box::new(SnailNum::Regular(r)));
            true
        }
        SnailNum::Pair(box x, box y) => {
            split(x, depth + 1) || split(y, depth + 1)
        }
        _ => false
    };
    // println!("split depth {} {:?} -> {}", depth, sn, ret);
    ret
}

fn magnitude(sn: &SnailNum) -> u32 {
    match sn {
        SnailNum::Pair(box x, box y) => 3*magnitude(x) + 2*magnitude(y),
        SnailNum::Regular(x) => *x,
    }
}