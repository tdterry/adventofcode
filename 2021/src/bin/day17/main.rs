use regex::Regex;

fn main() {
    // let input = include_str!("sample_input.txt");
    let input = include_str!("input.txt");
    let mut lines = input.lines();

    let re = Regex::new(r"target area: x=(\d+)..(\d+), y=(-\d+)..(-\d+)").unwrap();
    let text = lines.next().unwrap();
    for cap in re.captures_iter(text) {
        let x_min: isize = cap[1].parse().unwrap();
        let x_max: isize = cap[2].parse().unwrap();
        let y_min: isize = cap[3].parse().unwrap();
        let y_max: isize = cap[4].parse().unwrap();

        println!("x min {}, x max {}, y min {}, y max {}", x_min, x_max, y_min, y_max);

        let mut best_height = 0;
        // let mut solutions: Vec<(isize, isize)> = Vec::new();
        let mut num_solutions = 0;
        for x_vel_initial in 0..x_max+1 {
            for y_vel_initial in y_min..10000 {
                let mut pos = (0, 0);
                let mut x_vel = x_vel_initial;
                let mut y_vel = y_vel_initial;
                let mut solved = false;
                let mut max_height = 0;
                while pos.0 < x_max && pos.1 > y_min {
                    pos.0 += x_vel;
                    pos.1 += y_vel;
                    if pos.1 > max_height {
                        max_height = pos.1;
                    }
                    if pos.0 >= x_min && pos.0 <= x_max && pos.1 >= y_min && pos.1 <= y_max {
                        solved = true;
                        // solutions.push((x_vel_initial, y_vel_initial));
                        // println!("{},{}", x_vel_initial, y_vel_initial);
                        num_solutions += 1;
                        break;
                    }
                    x_vel -= 1;
                    y_vel -= 1;
                    if x_vel < 0 {
                        x_vel = 0;
                    }
                }

                if solved && max_height > best_height {
                    best_height = max_height;
                }
            }
        }

        println!("best height {}", best_height);
        println!("num solutions {}", num_solutions);

        // solutions.sort();
        // println!("{:?}", solutions);
    }
}

