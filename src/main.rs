mod processor_status;

use processor_status::*;

use std::fs;

const KB8: usize = 1024 * 8;
const KB16: usize = 1024 * 16;

struct CPU {
    program_counter: u16,
    stack_pointer: u8,
    accumulator: u8,
    x: u8,
    y: u8,
    status: StatusRegister,

    memory: Memory
}

struct Memory {
    upper_rom: Box<[u8]>,
    lower_rom: Box<[u8]>,
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
            status: StatusRegister::new(),

            memory: Memory::from_cartridge("roms/nestest.nes")
        }
    }

    fn execute_next_instruction() {
        todo!()
    }
}

impl Memory {
    fn new() -> Self {
        Memory {
            upper_rom: Box::new([0; 0x4000]),
            lower_rom: Box::new([0; 0x4000]),
            ram: Box::new([0; 0x800]),
        }
    }

    fn from_cartridge(path: &str) -> Self {
        let mut file = match fs::read(path){
            Ok(data) => data,
            Err(err) => panic!("{}", err)
        };
        
        let header = file.drain(0..16).collect::<Vec<u8>>();

        if header[0..4] != ['N' as u8, 'E' as u8, 'S' as u8, 0x1A] {
            panic!("Invalid cartridge")
        }

        let prg_rom_banks = header[4];
        let _vrom_banks = header[5];

        let prg_rom;
        if prg_rom_banks == 1 {
            prg_rom = file.drain(0..KB16).collect::<Box<[u8]>>();
        } else {
            panic!("More than 1 chunk of ram")
        };
        
        Memory {
            upper_rom: prg_rom.clone(),
            lower_rom: prg_rom,
            ram: Box::new([0; 0x800])
        }
    }

    fn read(&self, address: u16) -> u8 {
        if address < 0x2000 {
            self.ram[(address % 0x800) as usize]
        } else if address < 0x8000 {
            todo!("Read 0x2000 - 0x8000")
        } else if address < 0xC000 {
            self.lower_rom[address as usize - 0x8000]
        } else {
            self.upper_rom[address as usize - 0xC000]
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        if address < 0x2000 {
            self.ram[(address % 0x800) as usize] = value
        } else {
            todo!("Write > 0x2000")
        }
    }
}

fn main() {
    let cpu = CPU::new();
}