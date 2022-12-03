use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn get_priority(item: char) -> usize {
    match item as usize {
        n @ 65..=90 => n - 38,
        n @ 97..=122 => n - 96,
        _ => panic!(),
    }
}

fn calculate_priorities(items: &[char]) -> Vec<usize> {
    items.iter().map(|&item| get_priority(item)).collect()
}

fn find_duplicates(rucksack: &str) -> Vec<char> {
    let (compartment0, compartment1) = rucksack.split_at(rucksack.len() / 2);
    let (compartment0, compartment1): (HashSet<char>, HashSet<char>) = (
        HashSet::from_iter(compartment0.chars()),
        HashSet::from_iter(compartment1.chars()),
    );
    Vec::from_iter(&compartment0 & &compartment1)
}

fn find_badge(elves: &[String]) -> char {
    let (mut elf0, elf1, elf2): (_, HashSet<_>, HashSet<_>) = (
        elves[0].chars(),
        HashSet::from_iter(elves[1].chars()),
        HashSet::from_iter(elves[2].chars()),
    );

    let intersection = &elf1 & &elf2;

    elf0.find(|item| intersection.contains(item)).unwrap()
}

fn main() -> io::Result<()> {
    let input = File::open("input")?;
    let lines = BufReader::new(input)
        .lines()
        .map(Result::unwrap)
        .collect::<Vec<_>>();

    let part1: usize = lines
        .iter()
        .flat_map(|line| calculate_priorities(&find_duplicates(line)))
        .sum();
    println!("part 1: {}", part1);

    let part2: usize = lines
        .chunks(3)
        .map(|chunk| get_priority(find_badge(chunk)))
        .sum();
    println!("part 2: {}", part2);

    Ok(())
}
