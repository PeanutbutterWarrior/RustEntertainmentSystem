use crate::processor_status::*;

use std::fs;

const KB16: usize = 1024 * 16;

enum AddressingMode {
    None,
    Accumulator,
    Immediate(u8),
    Indirect(u16),
}

pub struct CPU {
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
    pub fn new() -> Self {
        CPU {
            program_counter: 0,
            stack_pointer: 0,
            accumulator: 0,
            x: 0,
            y: 0,
            status: StatusRegister::new(),

            memory: Memory::from_cartridge("roms/nestest.nes") // TODO make better
        }
    }

    pub fn execute_next_instruction(&mut self) {
        let opcode = self.next_byte();
        let instruction = ((opcode & 0b11100000) >> 3) + (opcode & 0b11);
        let addressing = (opcode & 0b00011100) >> 2;

        let addressing_mode = self.addressing_mode(instruction, addressing);
        
        match instruction {
            1 => self.accumulator |= self.value_of(addressing_mode).unwrap(), // ORA
            5 => self.accumulator &= self.value_of(addressing_mode).unwrap(), // AND
            9 => self.accumulator ^= self.value_of(addressing_mode).unwrap(), // EOR
            13 => {                                                           // ADC
                let (result, overflow) = self.accumulator.overflowing_add(self.value_of(addressing_mode).unwrap());
                let (result, overflow2) = result.overflowing_add(self.status.get(StatusBit::Carry) as u8);
                self.accumulator = result;
                self.status.set(StatusBit::Carry, overflow || overflow2)
            }
            _ => {}
        }
    }

    fn next_byte(&mut self) -> u8 {
        self.program_counter += 1;
        self.memory.read(self.program_counter - 1)
    }

    fn addressing_mode(&mut self, instruction: u8, addressing: u8) -> AddressingMode{
        if instruction & 1 == 0 {
            match addressing {
                0 => {
                    let index = self.x.wrapping_add(self.next_byte());
                    let address = self.memory.read_double(index as u16);
                    AddressingMode::Indirect(address)
                }
                1 => AddressingMode::Indirect(self.next_byte() as u16),
                2 => AddressingMode::Immediate(self.next_byte()),
                3 => AddressingMode::Indirect(u16::from_le_bytes([self.next_byte(), self.next_byte()])),
                4 => {
                    let next = self.next_byte();
                    let address = self.memory.read_double(next as u16);
                    AddressingMode::Indirect( address + self.y as u16)
                },
                5 => AddressingMode::Indirect(self.next_byte().wrapping_add(self.x) as u16),
                6 => AddressingMode::Indirect(u16::from_le_bytes([self.next_byte(), self.next_byte()]) + self.y as u16),
                7 => AddressingMode::Indirect(u16::from_le_bytes([self.next_byte(), self.next_byte()]) + self.x as u16),
                _ => { unreachable!() }
            }
        } else {
            todo!()
        }
    }

    fn value_of(&self, addressing_mode: AddressingMode) -> Option<u8> {
        match addressing_mode {
            AddressingMode::None => None,
            AddressingMode::Accumulator => Some(self.accumulator),
            AddressingMode::Immediate(value) => Some(value),
            AddressingMode::Indirect(value) => Some(self.memory.read(value))
        }
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

    fn read_double(&self, address: u16) -> u16 {
        u16::from_le_bytes([self.read(address), self.read(address + 1)])
    }

    fn write(&mut self, address: u16, value: u8) {
        if address < 0x2000 {
            self.ram[(address % 0x800) as usize] = value
        } else {
            todo!("Write > 0x2000")
        }
    }
}
