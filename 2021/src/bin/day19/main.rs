use std::collections::HashSet;
use std::str::Lines;
use num_traits::abs;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Point(isize, isize, isize);

#[derive(Debug, Clone)]
struct Scanner {
    points: HashSet<Point>,
    points_rot: Vec<HashSet<Point>>,
}

type Rotation = [[isize; 3]; 3];

fn matrix_mul(r0: &Rotation, r1: &Rotation) -> Rotation {
    let mut ret = [[0; 3]; 3];
    for j in 0..3 {
        for i in 0..3 {
            for k in 0..3 {
                ret[j][i] += r0[j][k] * r1[k][i];
            }
        }
    }
    ret
}

fn point_mul(r: &Rotation, pt: &Point) -> Point {
    // println!("point_mul r={:?} pt={:?}", r, pt);
    Point(
        pt.0 * r[0][0] + pt.1 * r[0][1] + pt.2 * r[0][2],
        pt.0 * r[1][0] + pt.1 * r[1][1] + pt.2 * r[1][2],
        pt.0 * r[2][0] + pt.1 * r[2][1] + pt.2 * r[2][2],
    )
}

fn point_add(pt: &Point, rel: &Point) -> Point {
    Point(
        pt.0 + rel.0,
        pt.1 + rel.1,
        pt.2 + rel.2,
    )
}

fn rot_z() -> Vec<Rotation> {
    let mut rots: Vec<Rotation> = Vec::new();
    // z rotations
    rots.push([
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1],
    ]);
    rots.push([
        [0, -1, 0],
        [1, 0, 0],
        [0, 0, 1],
    ]);
    rots.push([
        [-1, 0, 0],
        [0, -1, 0],
        [0, 0, 1],
    ]);
    rots.push([
        [0, 1, 0],
        [-1, 0, 0],
        [0, 0, 1],
    ]);
    rots
}

fn rot_y() -> Vec<Rotation> {
    let mut rots: Vec<Rotation> = Vec::new();
    // y rotations
    rots.push([
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1],
    ]);
    rots.push([
        [0, 0, -1],
        [0, 1, 0],
        [1, 0, 0],
    ]);
    rots.push([
        [-1, 0, 0],
        [0, 1, 0],
        [0, 0, -1],
    ]);

    rots.push([
        [0, 0, 1],
        [0, 1, 0],
        [-1, 0, 0],
    ]);
    rots
}

fn rot_x() -> Vec<Rotation> {
    let mut rots: Vec<Rotation> = Vec::new();
    // x rotations
    rots.push([
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1],
    ]);
    rots.push([
        [1, 0, 0],
        [0, 0, -1],
        [0, 1, 0],
    ]);
    rots.push([
        [1, 0, 0],
        [0, -1, 0],
        [0, 0, -1],
    ]);
    rots.push([
        [1, 0, 0],
        [0, 0, 1],
        [0, -1, 0],
    ]);
    rots
}

fn all_rotations() -> Vec<Rotation> {
    let mut rots: Vec<Rotation> = Vec::new();

    for z in rot_z() {
        for y in rot_y() {
            for x in rot_x() {
                let rot = matrix_mul(&z, &matrix_mul(&y, &x));
                if !rots.contains(&rot) {
                    rots.push(rot);
                }
            }
        }
    }

    rots
}

fn parse_scanner(rots: &[Rotation], lines: &mut Lines) -> Option<Scanner> {
    let mut points: HashSet<Point> = HashSet::new();
    while let Some(line) = lines.next() {
        if line == "" {
            break;
        } else if line.starts_with("---") {
            continue;
        }

        let mut parts = line.split(",");
        points.insert(Point(
            parts.next().unwrap().parse::<isize>().unwrap(),
            parts.next().unwrap().parse::<isize>().unwrap(),
            parts.next().unwrap().parse::<isize>().unwrap(),
        ));
    }

    let mut points_rot: Vec<HashSet<Point>> = Vec::new();
    for rot in rots {
        points_rot.push(points.iter().map(|p| point_mul(rot, p)).collect());
    }

    if points.len() > 0 {
        Some(Scanner { points, points_rot })
    } else {
        None
    }
}

fn main() {
    // let input = include_str!("sample_input.txt");
    // let input = include_str!("sample_input2.txt");
    let input = include_str!("input.txt");
    let mut lines = input.lines();

    let rots = all_rotations();
    let mut scanners: Vec<Scanner> = Vec::new();
    while let Some(scanner) = parse_scanner(&rots, &mut lines) {
        scanners.push(scanner);
    }

    // let mut solved_scanners = vec![scanners[0].clone()];
    let mut solved_scanner = scanners[0].clone();
    scanners = scanners[1..].iter().map(|s| s.clone()).collect();

    let mut scanner_positions: Vec<Point> = Vec::new();

    while scanners.len() > 0 {
        for i in 0..scanners.len() {
            let s = &mut scanners[i];
            let mut is_solved = false;
            let mut test_points: HashSet<Point> = HashSet::new();
            let mut rel = Point(0, 0, 0);
            for p0 in &solved_scanner.points {
                for points in &s.points_rot {
                    for p1 in points.iter() {
                        rel = Point(p0.0 - p1.0, p0.1 - p1.1, p0.2 - p1.2);
                        test_points = points.iter().map(|p| point_add(p, &rel)).collect();

                        let points_in_common = solved_scanner.points.intersection(&test_points);
                        if points_in_common.count() >= 12 {
                            println!("solution!");
                            is_solved = true;
                            break;
                        }
                    }
                    if is_solved { break; }
                }
                if is_solved { break; }
            }

            if is_solved {
                for p in &test_points {
                    solved_scanner.points.insert(p.clone());
                }
                scanner_positions.push(rel);
                scanners = scanners.iter().enumerate().filter(|&(j, s)| i != j).map(|(_, s)| s.clone()).collect();
                break;
            }
        }
    }

    dbg!(&solved_scanner);
    dbg!(&scanner_positions);

    println!("unique beacons {}", solved_scanner.points.len());

    let mut max_dist = 0;
    for s0 in &scanner_positions {
        for s1 in &scanner_positions {
            let manhattan_dist = abs(s0.0 - s1.0) + abs(s0.1 - s1.1) + abs(s0.2 - s1.2);
            if manhattan_dist > max_dist {
                max_dist = manhattan_dist
            }
        }
    }

    println!("max distance {}", max_dist);
}

