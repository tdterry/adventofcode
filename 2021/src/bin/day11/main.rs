fn main() {
    let input = include_str!("input.txt");
    let lines = input.lines();

    let board: Vec<Vec<u32>> = lines.map(|l| l.chars().map(|c| String::from(c).parse().unwrap()).collect()).collect();

    part1(&board);
    part2(&board);
}

fn part1(board: &Vec<Vec<u32>>) {
    let mut board = board.clone();
    let mut flashes = 0;
    for i in 1..101 {
        // println!("after step {}", i);
        let board_flashes = step(&board);
        board = board_flashes.0;
        flashes += board_flashes.1;
        // for r in &board {
        //     println!("{:?}", r);
        // }
        // println!();
    }

    for r in board {
        println!("{:?}", r);
    }
    println!("flashes {}", flashes);
}

fn part2(board: &Vec<Vec<u32>>) {
    let board_count = board.iter().map(|r| r.len()).sum();
    let mut board = board.clone();
    let mut flashes = 0;
    let mut i = 0;
    loop {
        i += 1;
        // println!("after step {}", i);
        let board_flashes = step(&board);
        board = board_flashes.0;
        flashes += board_flashes.1;
        if board_flashes.1 == board_count {
            break;
        }
        // for r in &board {
        //     println!("{:?}", r);
        // }
        // println!();
    }

    for r in board {
        println!("{:?}", r);
    }
    println!("synchronized at {}", i);
}

fn step(board: &Vec<Vec<u32>>) -> (Vec<Vec<u32>>, usize) {
    let mut settled = false;
    let mut next: Vec<Vec<u32>> = board.iter().map(|r| r.iter().map(|x| x + 1).collect()).collect();
    while !settled {
        settled = true;
        for y in 0..next.len() {
            for x in 0..next[0].len() {
                let mut neighbors: Vec<(usize, usize)> = vec![];
                if next[y][x] > 9 {
                    settled = false;

                    if y > 0 {
                        if x > 0 {
                            neighbors.push((x - 1, y - 1));
                        }
                        neighbors.push((x, y - 1));
                        if x < next[0].len() - 1 {
                            neighbors.push((x + 1, y - 1));
                        }
                    }
                    if x > 0 {
                        neighbors.push((x - 1, y));
                    }
                    if x < next[0].len() - 1 {
                        neighbors.push((x + 1, y));
                    }
                    if y < next.len() - 1 {
                        if x > 0 {
                            neighbors.push((x - 1, y + 1));
                        }
                        neighbors.push((x, y + 1));
                        if x < next[0].len() - 1 {
                            neighbors.push((x + 1, y + 1));
                        }
                    }

                    for &(x, y) in &neighbors {
                        if next[y][x] != 0 {
                            next[y][x] += 1;
                        }
                    }
                    next[y][x] = 0;
                }
            }
        }
    }

    let flashes = next.iter().map(|r| r.iter().filter(|&&x| x==0).count()).sum();

    (next, flashes)
}