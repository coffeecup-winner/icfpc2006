use std::{
    env, fs,
    io::{self, Read},
    path::Path,
};

mod um32;

fn as_mut_u8_slice(slice: &mut [u32]) -> &mut [u8] {
    unsafe {
        std::slice::from_raw_parts_mut(
            slice.as_mut_ptr() as *mut u8,
            slice.len() * std::mem::size_of::<u32>(),
        )
    }
}

fn read_into_vec_u32<P: AsRef<Path>>(path: &P) -> io::Result<Vec<u32>> {
    let size = fs::metadata(path)?.len();
    if size % 4 != 0 {
        panic!("Invalid UM program file");
    }
    let mut result = vec![0; (size / 4) as usize];
    fs::File::open(path)?.read(as_mut_u8_slice(&mut result))?;
    Ok(result)
}

fn main()  -> io::Result<()> {
    if env::args().len() != 2 {
        eprintln!("Usage: um32 <UM program file>");
        std::process::exit(1);
    }
    let program = read_into_vec_u32(&env::args().nth(1).unwrap())?;
    um32::run(program);
    Ok(())
}
