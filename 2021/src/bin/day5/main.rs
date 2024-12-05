use std::cmp::Ordering;
use std::fmt::Debug;
use std::str::FromStr;

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy)]
struct Point {
    x: usize,
    y: usize,
}

#[derive(Debug, Clone)]
struct Line {
    from: Point,
    to: Point,
}

impl<'a> IntoIterator for &'a Line {
    type Item = Point;
    type IntoIter = LineIterator<'a>;
    fn into_iter(self) -> Self::IntoIter {
        LineIterator::new(self)
    }
}

struct LineIterator<'a> {
    l: &'a Line,
    curr: Point,
    done: bool,
    x_dir: Ordering,
    y_dir: Ordering,
}

impl<'a> LineIterator<'a> {
    fn new(l: &'a Line) -> LineIterator<'a> {
        LineIterator {
            l,
            curr: l.from,
            done: false,
            x_dir: l.to.x.cmp(&l.from.x),
            y_dir: l.to.y.cmp(&l.from.y),
        }
    }
}

fn add_coord(x: usize, dir: Ordering) -> usize {
    match dir {
        Ordering::Greater => x+1,
        Ordering::Less => x-1,
        _ => x
    }
}

impl<'a> Iterator for LineIterator<'a> {
    type Item = Point;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let ret = Some(self.curr);
        if self.l.to == self.curr {
            self.done = true
        } else {
            self.curr = Point {
                x: add_coord(self.curr.x, self.x_dir),
                y: add_coord(self.curr.y, self.y_dir),
            };
        }
        ret
    }
}


impl FromStr for Line {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let points: Vec<&str> = s.split(" -> ").collect();
        let coord0: Vec<usize> = points[0].split(",").map(|p| p.parse().unwrap()).collect();
        let coord1: Vec<usize> = points[1].split(",").map(|p| p.parse().unwrap()).collect();
        Ok(Line {
            from: Point { x: coord0[0], y: coord0[1] },
            to: Point { x: coord1[0], y: coord1[1] },
        })
    }
}

type Grid = [[u32; 1000]; 1000];

fn part1(lines: &Vec<Line>) {
    let mut grid: Grid = [[0; 1000]; 1000];
    for l in lines {
        if l.from.x == l.to.x {
            // vertical
            for y in std::cmp::min(l.from.y, l.to.y)..std::cmp::max(l.from.y, l.to.y) + 1 {
                grid[y][l.from.x] += 1;
            }
        } else if l.from.y == l.to.y {
            // horizontal
            for x in std::cmp::min(l.from.x, l.to.x)..std::cmp::max(l.from.x, l.to.x) + 1 {
                grid[l.from.y][x] += 1;
            }
        }
    }

    let overlaps = count_if(|x| x > 1, &grid);
    println!("overlapping horizonal/vertical points: {}", overlaps);
    assert_eq!(overlaps, 6856);
}

fn part2(lines: &Vec<Line>) {
    let mut grid: Grid = [[0; 1000]; 1000];
    for l in lines {
        for p in l {
            grid[p.y][p.x] += 1;
        }
    }

    let overlaps = count_if(|x| x > 1, &grid);

    println!("overlapping diagonal points: {}", overlaps);
    assert_eq!(overlaps, 20666);
}

fn main() {
    let input = include_str!("input.txt");
    let lines = input.lines();
    let lines: Vec<Line> = lines.map(|l| l.parse().unwrap()).collect();

    part1(&lines);
    part2(&lines);
}

fn count_if<T: Fn(u32) -> bool>(p: T, grid: &Grid) -> usize {
    grid.iter().map(|r| r.iter().filter(|&&v| p(v)).count()).sum()
}