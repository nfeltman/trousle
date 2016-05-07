use std::fs::File;

mod nsf;
mod instructions;
mod nesmemory;
mod machine;

fn main() {
    println!("{}", std::mem::size_of::<instructions::Full6502::Instruction>());

    let f = File::open("../testfiles/simple.nsf");
    match f {
        Err(_) => {println!("Failed to open file.")},
        Ok(f) => {
            let header = nsf::read_nsf_header(f);
            println!("{:?}", header);
            // f is closed when it goes out of scope here  
        }
    }

}