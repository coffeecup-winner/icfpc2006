use std::io::{self, Read, Write};

pub fn run(program: Vec<u32>) -> io::Result<()> {
    let mut data = vec![program; 1];
    let mut free = vec![0usize; 0];
    let mut ip = 0;
    let mut registers = vec![0u32; 8];
    let stdin_ = io::stdin();
    let mut stdin = stdin_.lock();
    let stdout_ = io::stdout();
    let mut stdout = stdout_.lock();
    loop {
        let mut insn = data[0][ip];
        ip += 1;
        let op = insn >> 28;
        match op {
            // Conditional Move
            0 => {
                let c = insn & 0x7;
                if registers[c as usize] != 0 {
                    insn >>= 3;
                    let b = insn & 0x7;
                    insn >>= 3;
                    let a = insn & 0x7;
                    registers[a as usize] = registers[b as usize];
                }
            }
            // Array Index
            1 => {
                let c = insn & 0x7;
                insn >>= 3;
                let b = insn & 0x7;
                insn >>= 3;
                let a = insn & 0x7;
                registers[a as usize] =
                    data[registers[b as usize] as usize][registers[c as usize] as usize];
            }
            // Array Amendment
            2 => {
                let c = insn & 0x7;
                insn >>= 3;
                let b = insn & 0x7;
                insn >>= 3;
                let a = insn & 0x7;
                data[registers[a as usize] as usize][registers[b as usize] as usize] =
                    registers[c as usize];
            }
            // Addition
            3 => {
                let c = insn & 0x7;
                insn >>= 3;
                let b = insn & 0x7;
                insn >>= 3;
                let a = insn & 0x7;
                registers[a as usize] = registers[b as usize].wrapping_add(registers[c as usize]);
            }
            // Multiplication
            4 => {
                let c = insn & 0x7;
                insn >>= 3;
                let b = insn & 0x7;
                insn >>= 3;
                let a = insn & 0x7;
                registers[a as usize] = registers[b as usize].wrapping_mul(registers[c as usize]);
            }
            // Division
            5 => {
                let c = insn & 0x7;
                insn >>= 3;
                let b = insn & 0x7;
                insn >>= 3;
                let a = insn & 0x7;
                registers[a as usize] = registers[b as usize] / registers[c as usize];
            }
            // Not-And
            6 => {
                let c = insn & 0x7;
                insn >>= 3;
                let b = insn & 0x7;
                insn >>= 3;
                let a = insn & 0x7;
                registers[a as usize] = !(registers[b as usize] & registers[c as usize]);
            }
            // Halt
            7 => break,
            // Allocation
            8 => {
                let c = insn & 0x7;
                insn >>= 3;
                let b = insn & 0x7;
                let new_arr = vec![0; registers[c as usize] as usize];
                let new_idx = if let Some(idx) = free.pop() {
                    data[idx] = new_arr;
                    idx
                } else {
                    data.push(new_arr);
                    data.len() - 1
                };
                registers[b as usize] = new_idx as u32;
            }
            // Abandonment
            9 => {
                let c = insn & 0x7;
                let idx = registers[c as usize] as usize;
                data[idx].clear();
                free.push(idx);
            }
            // Output
            10 => {
                let c = insn & 0x7;
                let v = registers[c as usize] as u8;
                stdout.write_all(&[v])?;
                stdout.flush()?;
            }
            // Input
            11 => {
                let c = insn & 0x7;
                let mut buf = [0; 1];
                let res = stdin.read(&mut buf)?;
                if res == 0 {
                    registers[c as usize] = !0;
                } else {
                    registers[c as usize] = buf[0] as u32;
                }
            }
            // Load Program
            12 => {
                let c = insn & 0x7;
                insn >>= 3;
                let b = insn & 0x7;
                match registers[b as usize] {
                    0 => {}
                    v => data[0] = data[v as usize].clone(),
                }
                ip = registers[c as usize] as usize;
            }
            // Orthography
            13 => {
                let a = (insn >> 25) & 0x7;
                registers[a as usize] = insn & 0b0000_0001_1111_1111_1111_1111_1111_1111;
            }
            _ => panic!("Invalid operation {}", op),
        }
    }
    Ok(())
}
