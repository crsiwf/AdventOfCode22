use std::{
    collections::HashSet,
    fs::File,
    io::{self, Read},
};

type Position = (isize, isize);
type Direction = (isize, isize);

struct Motion {
    direction: Direction,
    count: usize,
}

fn parse_commands(input: &str) -> Vec<Motion> {
    input
        .lines()
        .map(|line| {
            let (direction, count) = line.split_once(' ').unwrap();
            let direction = match direction {
                "U" => (0, 1),
                "D" => (0, -1),
                "L" => (-1, 0),
                "R" => (1, 0),
                _ => panic!(),
            };
            let count = count.parse().unwrap();
            Motion { direction, count }
        })
        .collect()
}

fn translate(position: Position, direction: Direction) -> Position {
    (position.0 + direction.0, position.1 + direction.1)
}

fn update_tail(head: Position, tail: Position) -> Position {
    let clamp = |(x, y)| ((-1).max(1.min(x)), (-1).max(1.min(y)));
    let difference = (head.0 - tail.0, head.1 - tail.1);
    if difference.0.abs() >= 2 || difference.1.abs() >= 2 {
        translate(tail, clamp(difference))
    } else {
        tail
    }
}

fn simulate_rope(motions: &Vec<Motion>, length: usize) -> usize {
    let mut rope = vec![(0, 0); length];
    let mut tail_visited = HashSet::<Position>::from([(0, 0)]);

    for motion in motions {
        for _ in 0..motion.count {
            rope[0] = translate(rope[0], motion.direction);
            for i in 1..length {
                rope[i] = update_tail(rope[i - 1], rope[i]);
            }
            tail_visited.insert(rope[length - 1]);
        }
    }
    tail_visited.len()
}

fn main() -> io::Result<()> {
    let mut file = File::open("input")?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;

    let commands = parse_commands(&input);
    println!("part 1: {:?}", simulate_rope(&commands, 2));
    println!("part 2: {:?}", simulate_rope(&commands, 10));

    Ok(())
}
