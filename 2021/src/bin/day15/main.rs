use std::collections::VecDeque;

fn main() {
    let input = include_str!("input.txt");
    let mut lines = input.lines();
    let mut board: Vec<Vec<u64>> = vec![];
    let mut expanded_board: Vec<Vec<u64>> = vec![];
    let mut visited: Vec<Vec<(u64, (usize, usize))>> = vec![];
    let mut expanded_visited: Vec<Vec<(u64, (usize, usize))>> = vec![];

    let mut width = 0;
    let mut height = 0;
    for line in lines {
        println!("{}", line);
        let row: Vec<u64> = line.chars().map(|x| String::from(x).parse().unwrap()).collect();
        let mut expanded_row: Vec<u64> = Vec::new();
        for i in 0..5 {
            row.iter().for_each(|x| expanded_row.push(if (x + i) > 9 { x + i - 9 } else { x + i }));
        }

        let visit_row = (0..row.len()).map(|_| (u64::MAX, (0, 0))).collect();
        let expanded_visit_row = (0..expanded_row.len()).map(|_| (u64::MAX, (0, 0))).collect();
        width = row.len();
        board.push(row);
        expanded_board.push(expanded_row);
        visited.push(visit_row);
        expanded_visited.push(expanded_visit_row);
    }
    height = board.len();

    for i in 1..5 {
        for row in expanded_board.clone().iter().take(height) {
            println!("expanding");
            // expand down
            let expanded_row = row.iter().map(|x| if (x + i) > 9 { x + i - 9 } else { x + i }).collect();
            expanded_board.push(expanded_row);
            expanded_visited.push(expanded_visited[0].clone());
        }
    }

    visited[0][0] = (0, (0, 0));
    shortest_path(&board, &mut visited, &(0, 0), &(width, height));
    println!("min risk {}", visited[height - 1][width - 1].0);

    for r in &expanded_board {
        println!("{:?}", r);
    }

    expanded_visited[0][0] = (0, (0, 0));
    shortest_path(&expanded_board, &mut expanded_visited, &(0, 0), &(width*5, height*5));
    println!("min risk expanded {}", expanded_visited[height*5 - 1][width * 5 - 1].0);
}

fn shortest_path(
    board: &Vec<Vec<u64>>,
    visited: &mut Vec<Vec<(u64, (usize, usize))>>,
    curr: &(usize, usize),
    dest: &(usize, usize),
) {
    let mut visit_next: VecDeque<(usize, usize)> = VecDeque::new();
    visit_next.push_back(*curr);

    while let Some((x, y)) = visit_next.pop_front() {
        // println!("{:?}", (x, y));
        let mut steps: Vec<(usize, usize)> = vec![];
        if x > 0 { steps.push((x - 1, y)); }
        if y > 0 { steps.push((x, y - 1)); }
        if x < dest.0 - 1 { steps.push((x + 1, y)); }
        if y < dest.1 - 1 { steps.push((x, y + 1)); }
        for (x_next, y_next) in steps {
            if visited[y][x].0 + board[y_next][x_next] < visited[y_next][x_next].0 {
                visited[y_next][x_next] = (visited[y][x].0 + board[y_next][x_next], (x, y));
                visit_next.push_back((x_next, y_next));
            }
        }
    }
}