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

struct Memory {
    ram: Box<[u8]>,
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

impl Memory {
    fn new() -> Self{
        Memory {
            ram: Box::new([0; 0x800]),
        }
    }

    fn read(&self, address: u16) -> u8 {
        if address < 0x2000 {
            self.ram[(address % 0x800) as usize]
        } else {
            0
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        if address < 0x2000 {
            self.ram[(address % 0x800) as usize] = value
        }
    }
}

fn main() {
    let cpu = CPU::new();
}