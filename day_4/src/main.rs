use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

#[derive(Debug)]
struct Assignment {
    start: usize,
    end: usize,
}

impl Assignment {
    fn contains(&self, other: &Assignment) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    fn overlaps(&self, other: &Assignment) -> bool {
        self.start <= other.start && self.end >= other.start
            || self.start >= other.start && self.start <= other.end
    }

    fn from_str(s: &str) -> Assignment {
        let mut split = s.split("-");
        Assignment {
            start: split.next().unwrap().parse().unwrap(),
            end: split.next().unwrap().parse().unwrap(),
        }
    }
}

fn fully_contained(assignments: &Vec<Assignment>) -> bool {
    assignments[0].contains(&assignments[1]) || assignments[1].contains(&assignments[0])
}

fn parse_assignments(line: &str) -> Vec<Assignment> {
    line.split(",")
        .map(Assignment::from_str)
        .collect::<Vec<_>>()
}

fn main() -> io::Result<()> {
    let input = File::open("input")?;
    let lines = BufReader::new(input)
        .lines()
        .map(Result::unwrap)
        .collect::<Vec<_>>();

    let part1: usize = lines
        .iter()
        .map(|l| fully_contained(&parse_assignments(&l)) as usize)
        .sum();
    println!("part 1: {}", part1);

    let part2: usize = lines
        .iter()
        .map(|l| {
            let a = parse_assignments(&l);
            a[0].overlaps(&a[1]) as usize
        })
        .sum();
    println!("part 2: {}", part2);

    Ok(())
}
