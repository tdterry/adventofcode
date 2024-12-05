use std::cmp::max;

fn main() {
    let input = include_str!("input.txt");

    let mut points: Vec<(u32, u32)> = vec![];

    let mut line_iter = input.lines();
    while let Some(line) = line_iter.next() {
        if line == "" {
            break;
        }
        let v: Vec<u32> = line.split(",").map(|v| v.parse().unwrap()).collect();
        points.push((v[0], v[1]));
    }

    println!("{:?}", points);
    while let Some(line) = line_iter.next() {
        let parts: Vec<_> = line.split(" ").collect();
        let axis_num: Vec<_> = parts[2].split("=").collect();
        let num: u32 = axis_num[1].parse().unwrap();
        println!("{} {}", axis_num[0], num);

        let mut new_points: Vec<(u32, u32)> = vec![];
        for mut p in points.iter_mut() {
            if axis_num[0] == "y" {
                // fold y
                if p.1 <= num {
                    // keep
                } else {
                    // fold
                    p.1 = num - (p.1 - num);
                }
                if !new_points.contains(p) {
                    new_points.push(*p);
                }
            } else {
                // fold x
                if p.0 < num {
                    // keep
                } else if p.0 > num {
                    // fold
                    p.0 = num - (p.0 - num);
                }
                if !new_points.contains(p) {
                    new_points.push(*p);
                }
            }
        }
        points = new_points;
        // println!("{:?}", points);
        // break;
    }

    let (width,height) = points.iter().fold((0,0), |(x,y), &v| (max(x, v.0), max(y, v.1)));
    println!("width, height {},{}", width, height);
    println!("num points {}", points.len());

    for y in 0..height+1 {
        let mut row = String::from("");
        for x in 0..width+1 {
            if points.contains(&(x,y)) {
                row = row + "#"
            } else {
                row = row + "."
            }
        }
        println!("{}", row);
    }
}