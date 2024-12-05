use std::collections::HashSet;
use std::ops::Index;
use std::str::Lines;
use num_traits::abs;


fn main() {
    // let input = include_str!("sample_input.txt");
    let input = include_str!("input.txt");
    let mut lines = input.lines();

    let algo = lines.next().unwrap();
    println!("{}", algo);
    println!();

    lines.next(); // skip blank

    let mut image: Vec<String> = Vec::new();
    while let Some(l) = lines.next() {
        image.push(l.into());
    }

    // for r in &image {
    //     println!("{}", r);
    // }

    image = pad_image(&image);
    for r in &image {
        println!("{}", r);
    }

    for _ in 0..200 {
        image = pad_image(&image);
    }

    for i in 0..50 {
        image = enhance_image(algo, &image, i%2 == 1);
        for r in &image {
            println!("{}", r);
        }
    }
    let count: usize = image[100..image.len() - 100].iter().map(|r| r[100..r.len() - 100].chars().filter(|&ch| ch == '#').count()).sum();
    println!("pixels {}", count);
}

fn pad_image(img: &Vec<String>) -> Vec<String> {
    let height = img.len();
    let width = img[0].len();

    let mut ret: Vec<String> = Vec::new();
    ret.push(".".repeat(width + 2));
    for row in img {
        ret.push(".".to_owned() + row + ".");
    }
    ret.push(".".repeat(width + 2));

    ret
}

fn enhance_image(algo: &str, img: &Vec<String>, is_odd: bool) -> Vec<String> {
    let img = pad_image(img);
    let mut new_img: Vec<String> = Vec::new();// = img.iter().map(|r| ".".repeat(r.len())).collect();
    let pad = if is_odd { "#" } else { "." };
    new_img.push(pad.repeat(img[0].len()));

    for y in 0..img.len() - 2 {
        let mut row = pad.to_string();
        for x in 0..img[0].len() - 2 {
            let mut n = 0;
            for j in 0..3 {
                for i in 0..3 {
                    let ch = &img[y + j][x + i..x + i + 1];
                    // println!("{},{} is {}", x+i, y+j, ch);
                    n = (n << 1) + usize::from(ch == "#");
                }
            }

            if y == 0 {
                print!("{},{} {} ", x + 1, y + 1, n);
            }

            row += &algo[n..n + 1];
            // println!("{},{} {:b}", x, y, n);
            // new_img[y+1] = String::from(&new_img[y+1][..x+1]) + &algo[n..n+1] + &new_img[y+1][x+2..];
        }
        new_img.push(row + pad);
        if y == 0 {
            println!();
        }
    }

    new_img.push(pad.repeat(img[0].len()));
    new_img
}
