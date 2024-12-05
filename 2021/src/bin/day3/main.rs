use std::str::FromStr;

#[derive(Debug,PartialEq,Clone,Copy)]
enum Bit {
    One,
    Zero,
}

impl From<char> for Bit {
    fn from(c: char) -> Self {
        match c {
            '1' => Self::One,
            '0' => Self::Zero,
            c => panic!("{} is not a valid Bit", c),
        }
    }
}

impl From<bool> for Bit {
    fn from(b: bool) -> Self {
        match b {
            true => Self::One,
            false => Self::Zero,
        }
    }
}

impl From<Bit> for u32 {
    fn from(b: Bit) -> Self {
        match b {
            Bit::One => 1,
            Bit::Zero => 0,
        }
    }
}

#[derive(Debug)]
struct Num(Vec<Bit>);

impl FromStr for Num {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.chars().map(Bit::from).collect()))
    }
}

impl Clone for Num {
    fn clone(&self) -> Self {
        return Num(self.0[..].to_vec())
    }
}

impl From<Num> for u32 {
    fn from(n: Num) -> Self {
        Self::from(&n)
    }
}

impl From<&Num> for u32 {
    fn from(n: &Num) -> Self {
        n.0.iter().fold(0, |acc,&x| (acc << 1) + u32::from(x))
    }
}

fn count_at(v: &Vec<Num>, i: usize) -> (usize, usize) {
    v.iter().fold((0,0), |acc, x| match x {
        Num(b) if b[i] == Bit::One => (acc.0, acc.1 + 1),
        Num(b) if b[i] == Bit::Zero => (acc.0+1, acc.1),
        _ => acc,
    })
}

fn part1(lines: &Vec<Num>) {
    let counts: Vec<(usize, usize)> = (0..12).map(|i| count_at(lines, i)).collect();
    println!("counts={:?}", counts);

    let gamma = Num(counts.iter().map(|&(zeros, ones)| Bit::from(ones > zeros)).collect());
    let epsilon = Num(counts.iter().map(|&(zeros, ones)| Bit::from(zeros > ones)).collect());
    println!("gamma={:?} epsilon={:?}", gamma, epsilon);
    let answer = u32::from(gamma) * u32::from(epsilon);
    println!("{}", answer);
    assert_eq!(answer, 3813416);
}

fn find_oxy(lines: Vec<Num>, i: usize) -> Num {
    match lines.len() {
        1 => lines[0].clone(),
        _ => {
            let (zeros, ones) = count_at(&lines, i);
            find_oxy(lines.into_iter().filter(|x| x.0[i] == Bit::from(ones >= zeros)).collect::<Vec<Num>>().clone(), i+1)
        }
    }
}

fn find_co2(lines: Vec<Num>, i: usize) -> Num {
    let mut co2_scrubbers = lines.clone();
    for i in 0..12 {
        let (zeros, ones) = count_at(&co2_scrubbers, i);
        println!("{:?}, {}, {:?}", co2_scrubbers, i, (zeros, ones));
        co2_scrubbers = co2_scrubbers.into_iter().filter(|x| x.0[i] == Bit::from(zeros >= ones)).collect();
    }
    co2_scrubbers[0].clone()
}

fn part2(lines: &Vec<Num>) {
    let oxy_generator = find_oxy(lines.to_vec(), 0);
    let mut co2_scrubbers = lines.to_vec();
    let co2_scrubber = find_co2(lines.to_vec(), 0);

    for i in 0..12 {
        let (zeros, ones) = count_at(&co2_scrubbers, i);
        co2_scrubbers = co2_scrubbers.into_iter().filter(|x| x.0[i] == Bit::from(zeros > ones)).collect();
        if co2_scrubbers.len() == 1 {
            break
        }
    }

    println!("oxy_generator={:?}", oxy_generator);
    println!("co2_scrubbers={:?} ={}", co2_scrubbers, u32::from(&co2_scrubbers[0]));
    println!("co2_scrubber={:?} ={}", co2_scrubber, u32::from(&co2_scrubber));
    let answer = u32::from(&oxy_generator) * u32::from(&co2_scrubber);
    println!("{}", answer);
    assert_eq!(answer, 2990784);
}

fn main() {
    let input = include_str!("input.txt");
    let lines: Vec<Num> = input
        .lines()
        .map(|s| s.parse().unwrap())
        .collect();

    // println!("{:?}", commands);

    part1(&lines);
    part2(&lines);
}
