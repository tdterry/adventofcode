use num_traits::ToPrimitive;

#[derive(Debug)]
struct Board(Vec<Vec<u32>>);

fn find_basin(board: &Board, low_point: (usize,usize)) -> u32 {
    // println!("find_basin: {},{}", low_point.0, low_point.1);
    let mut in_basin: Vec<Vec<bool>> = board.0.iter().map(|r| r.iter().map(|_| false).collect()).collect();

    let (x, y) = low_point;
    in_basin[y][x] = true;

    let mut done = false;
    while !done {
        // println!("iteration");
        done = true;
        for y in 1..board.0.len()-1 {
            for x in 1..board.0[0].len()-1 {
                let height = board.0[y][x];
                if !in_basin[y][x] && height < 9 {
                    // see if it should be
                    if
                        in_basin[y+1][x] && height > board.0[y+1][x] ||
                        in_basin[y-1][x] && height > board.0[y-1][x] ||
                        in_basin[y][x+1] && height > board.0[y][x+1] ||
                        in_basin[y][x-1] && height > board.0[y][x-1]
                    {
                        in_basin[y][x] = true;
                        done = false;
                    }
                }
            }
        }
        // for r in in_basin.clone() {
        //     println!("{:?}", r);
        // }
    }

    let size:usize = in_basin.iter().map(|r| r.iter().filter(|&&v| v).count()).sum();
    let size = size.to_u32().unwrap();
    println!("basin for {},{} is {}", x, y, size);
    size
}

fn main() {
    let input = include_str!("input.txt");

    let mut width = 0;
    let mut board = Board(vec![]);
    let mut first = true;
    for line in input.lines() {
        let mut v = vec![10u32];
        for char in line.chars() {
            v.push(u32::from(char) - u32::from('0'));
        }
        v.push(10u32);

        if first {
            // add a dummy row
            width = v.len();
            board.0.push((0..width).map(|_| 10u32).collect());
            first = false;
        }

        board.0.push(v);
    }
    board.0.push((0..width).map(|_| 10u32).collect());
    let height = board.0.len();

    let mut low_points: Vec<(usize,usize)> = vec![];
    for y in 1..height-1 {
        for x in 1..width-1 {
            let point = board.0[y][x];
            if point < board.0[y-1][x] &&
            point < board.0[y+1][x] &&
            point < board.0[y][x-1] &&
            point < board.0[y][x+1] {
                low_points.push((x,y));
            }
        }
    }

    let risk:u32 = low_points.iter().map(|&(x,y)| board.0[y][x]+1).sum();
    println!("risk {}", risk);

    let mut basins: Vec<u32> = low_points.iter().map(|&p| find_basin(&board, p)).collect();

    basins.sort_by(|a,b| b.cmp(a));
    let basin_size: u32 = basins.iter().take(3).product();
    println!("basins {}", basin_size);

}