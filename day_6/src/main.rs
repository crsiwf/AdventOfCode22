use std::{
    fs::File,
    io::{self, Read}, collections::HashSet,
};

fn find_marker(input: &str, marker_length: usize) -> Option<usize> {
    let input = input.chars().collect::<Vec<_>>();
    let mut unique_chars: HashSet<char> = HashSet::new();

    for p in 0..=(input.len() - marker_length) {
        unique_chars.extend(input[p..p+marker_length].iter());
        if unique_chars.len() == marker_length {
            return Some(p + marker_length)
        }
        unique_chars.clear();
    }
    return None;
}

fn main() -> io::Result<()> {
    let mut file = File::open("input")?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;

    println!("part 1: {:?}", find_marker(&input, 4));

    println!("part 2: {:?}", find_marker(&input, 14));

    Ok(())
}
