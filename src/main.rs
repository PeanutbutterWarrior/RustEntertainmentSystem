mod processor_status;

use processor_status::*;

struct CPU {
    program_counter: u16,
    stack_pointer: u8,
    accumulator: u8,
    x: u8,
    y: u8,
    status: StatusRegister,
}

impl CPU {
    fn new() -> Self {
        CPU {
            program_counter: 0,
            stack_pointer: 0,
            accumulator: 0,
            x: 0,
            y: 0,
            status: StatusRegister::new() 
        }
    }

    fn execute_next_instruction() {
        todo!()
    }
}

fn main() {
    let cpu = CPU::new();
}