use std::{env, fs, io, path::Path};

mod um32;

fn read_into_vec_u32<P: AsRef<Path>>(path: &P) -> io::Result<Vec<u32>> {
    let data = fs::read(path)?;
    if data.len() % 4 != 0 {
        panic!("Invalid UM program file");
    }
    let mut result = Vec::with_capacity((data.len() / 4) as usize);
    let mut idx = 0;
    while idx < data.len() {
        let v = (data[idx] as u32) << 24
            | (data[idx + 1] as u32) << 16
            | (data[idx + 2] as u32) << 8
            | data[idx + 3] as u32;
        result.push(v);
        idx += 4;
    }
    Ok(result)
}

fn main() -> io::Result<()> {
    if env::args().len() != 2 {
        eprintln!("Usage: um32 <UM program file>");
        std::process::exit(1);
    }
    let program = read_into_vec_u32(&env::args().nth(1).unwrap())?;
    um32::run(program)
}
