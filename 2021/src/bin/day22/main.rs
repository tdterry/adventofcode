use std::any::Any;
use std::cmp::{max, min};
use std::fmt::Debug;
use std::ops::Range;
use std::str::FromStr;
use regex::Regex;

fn main() {
    let input = include_str!("sample_input.txt");
    // let input = include_str!("sample_input2.txt");
    // let input = include_str!("input.txt");
    let lines = input.lines();

    let cmds: Vec<Command> = lines.map(|l| l.parse()).filter(|c| c.is_ok()).map(|c| c.unwrap()).collect();
    dbg!(&cmds);

    let mut board = Board::default();

    for c in &cmds {
        let Command{x_range, y_range, z_range, ..} = c;
        for x in x_range.clone() {
            for y in y_range.clone() {
                for z in z_range.clone() {
                    // println!("{},{},{} {}", &x, &y, &z, c.on_off);
                    board.cubes[(z + 50) as usize][(y + 50) as usize][(x + 50) as usize] = c.on_off;
                }
            }
        }
    }

    let count_on: usize = board.cubes.iter().map(|z| z.iter().map(|y| y.iter().filter(|&&x| x).count()).sum::<usize>()).sum();
    println!("cubes on {}", count_on);
}

#[derive(Debug)]
struct Command {
    x_range: Range<isize>,
    y_range: Range<isize>,
    z_range: Range<isize>,
    on_off: bool,
}

#[derive(Debug)]
struct Board {
    cubes: [[[bool; 101]; 101]; 101],
}

impl Default for Board {
    fn default() -> Self {
        Board {
            cubes: [[[false; 101]; 101]; 101],
        }
    }
}

impl FromStr for Command {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let regex = Regex::new(r"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)").unwrap();
        let captures = regex.captures(s);
        for cap in captures {
            let on_off = &cap[1] == "on";
            let x_min: isize = cap[2].parse().unwrap();
            let x_max: isize = cap[3].parse().unwrap();
            let y_min: isize = cap[4].parse().unwrap();
            let y_max: isize = cap[5].parse().unwrap();
            let z_min: isize = cap[6].parse().unwrap();
            let z_max: isize = cap[7].parse().unwrap();
            println!("{} {} {} {} {} {} {} ", on_off, x_min, x_max, y_min, y_max, z_min, z_max);

            if x_min < -50 || x_max > 50 {
                return Err("out of bounds".into());
            }

            return Ok(Self {
                x_range: x_min..x_max+1,
                y_range: y_min..y_max+1,
                z_range: z_min..z_max+1,
                on_off,
            });
        }
        Err("nope".into())
    }
}

#[derive(Debug,Clone)]
struct Point3 { x: i64, y: i64, z: i64 }

#[derive(Debug,Clone)]
struct Cube {
    p1: Point3,
    p2: Point3,
}

fn overlap(c1: &Cube, c2: &Cube) -> Option<Cube>{
    let p1 = Point3{
        x: max(c1.p1.x, c1.p2.x),
        y: max(c1.p1.y, c1.p2.y),
        z: max(c1.p1.z, c1.p2.z),
    };
    let p2 = Point3 {
        x: min(c2.p1.x, c2.p2.x),
        y: min(c2.p1.y, c2.p2.y),
        z: min(c2.p1.z, c2.p2.z),

    };

    if p1.x < p2.x && p1.y < p2.y && p1.z < p2.z {
        Some(Cube{p1, p2})
    } else {
        None
    }
}

fn cube_add(c1: &Cube, c2: &Cube) -> Vec<Cube> {
    match overlap(c1, c2) {
        None => vec![c1.clone(), c2.clone()],
        Some(c3) => {
            vec![
                c1.clone(),
                // parts of c2 without c3
                Cube{
                    p1: Point3{
                        x: c3.p2.x,
                        y: c3.p1.y,
                        z: c3.p1.z,
                    },
                    p2: c3.p2.clone(),
                },
                Cube{
                    p1: Point3{
                        x: c3.p1.x,
                        y: c3.p2.y,
                        z: c3.p1.z,
                    },
                    p2: Point3{
                        x: c3.p2.x,
                        y: c2.p2.y,
                        z: c2.p2.z,
                    },
                },
                Cube{
                    p1: Point3{
                        x: c3.p1.x,
                        y: c3.p1.y,
                        z: c3.p2.z,
                    },
                    p2: Point3{
                        x: c3.p2.x,
                        y: c3.p2.y,
                        z: c2.p2.z
                    },
                }
            ]
        }
    }
}

fn cube_sub(c1: &Cube, c2: &Cube) -> Vec<Cube> {
    match overlap(c1, c2) {
        None => vec![c1.clone()],
        Some(c3) => {
            vec![
                // parts of c1 without c3
                Cube {
                    p1: c1.p1.clone(),
                    p2: Point3{
                        x: c1.p2.x,
                        y: c1.p2.y,
                        z: c3.p1.z,
                    },
                },
                Cube {
                    p1: Point3{
                        x: c1.p1.x,
                        y: c1.p1.y,
                        z: c3.p1.z,
                    },
                    p2: Point3{
                        x: c1.p2.x,
                        y: c1.p2.y,
                        z: c3.p1.z,
                    },
                },
                Cube {
                    p1: Point3{
                        x:
                        y:
                        z:
                    },
                    p2: Point3{
                        x:
                        y:
                        z:
                    },
                },
            ]
        }
    }
}
