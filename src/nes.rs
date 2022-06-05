use crate::processor_status::*;

use std::fs;

const KB16: usize = 1024 * 16;

enum AddressingMode {
    Implied,
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
        let operand = self.get_addressing_mode(opcode);
        
        if opcode & 0b111_000_11 == 0b000_000_01 { // ORA
            self.accumulator |= self.value_of(operand).unwrap();
            self.status.set(StatusBit::Negative, self.accumulator > 127);
            self.status.set(StatusBit::Zero, self.accumulator == 0);

        } else if opcode & 0b111_000_11 == 0b001_000_01 { // AND
            self.accumulator &= self.value_of(operand).unwrap();
            self.status.set(StatusBit::Negative, self.accumulator > 127);
            self.status.set(StatusBit::Zero, self.accumulator == 0);
        
        } else if opcode & 0b111_000_11 == 0b010_000_01 { // EOR
            self.accumulator ^= self.value_of(operand).unwrap();
            self.status.set(StatusBit::Negative, self.accumulator > 127);
            self.status.set(StatusBit::Zero, self.accumulator == 0);

        } else if opcode & 0b111_000_11 == 0b011_000_01 { // ADC
            let value = self.value_of(operand).unwrap();
            let (mut result, mut carry) = self.accumulator.overflowing_add(value);
            let mut overflow = (self.accumulator ^ result) & (value ^ result) & 0x80 != 0; // From http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
            
            if self.status.get(StatusBit::Carry) {
                result = result.wrapping_add(1);
                if result == 0 {
                    carry = true;
                    overflow = true;
                } else if result == 128 {
                    overflow = true;
                }
            }
            self.accumulator = result;
            self.status.set(StatusBit::Carry, carry);
            self.status.set(StatusBit::Overflow, overflow);
            self.status.set(StatusBit::Negative, self.accumulator > 127);
            self.status.set(StatusBit::Zero, self.accumulator == 0);
        
        } else if opcode & 0b111_000_11 == 0b100_000_01 { // STA
            if let AddressingMode::Indirect(addr) = operand {
                self.memory.write(addr, self.accumulator)
            }

        } else if opcode & 0b111_000_11 == 0b101_000_01 { // LDA
            self.accumulator = self.value_of(operand).unwrap();
            self.status.set(StatusBit::Negative, self.accumulator > 127);
            self.status.set(StatusBit::Zero, self.accumulator == 0);
        
        } else if opcode & 0b111_000_11 == 0b110_000_01 { // CMP
            let (result, carry) = self.value_of(operand).unwrap().overflowing_sub(self.accumulator);
            self.status.set(StatusBit::Carry, carry);
            self.status.set(StatusBit::Negative, result > 127);
            self.status.set(StatusBit::Zero, result == 0);
            self.accumulator = result
        } else if opcode & 0b111_000_11 == 0b111_000_01 { //SBC
            let value = !self.value_of(operand).unwrap();
            let (mut result, mut carry) = self.accumulator.overflowing_add(value);
            let mut overflow = (self.accumulator ^ result) & (value ^ result) & 0x80 != 0; // From http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
            
            if self.status.get(StatusBit::Carry) {
                result = result.wrapping_add(1);
                if result == 0 {
                    carry = true;
                    overflow = true;
                } else if result == 128 {
                    overflow = true;
                }
            }
            self.accumulator = result;
            self.status.set(StatusBit::Carry, carry);
            self.status.set(StatusBit::Overflow, overflow);
            self.status.set(StatusBit::Negative, self.accumulator > 127);
            self.status.set(StatusBit::Zero, self.accumulator == 0);
        }
    
    }

    fn next_byte(&mut self) -> u8 {
        self.program_counter += 1;
        self.memory.read(self.program_counter - 1)
    }

    fn get_addressing_mode(&mut self, opcode: u8) -> AddressingMode{
        let a = opcode & 0b11100000 >> 5;
        let b = opcode & 0b11100 >> 2;
        let c = opcode & 0b11;

        match b {
            0 => { 
                if c & 1 == 0 {
                    if a & 0b100 == 0{
                        if a == 1 {
                            AddressingMode::Indirect(u16::from(self.next_byte()) + u16::from(self.next_byte()) << 8)
                        } else {
                            AddressingMode::Implied
                        }
                    } else {
                        AddressingMode::Immediate(self.next_byte())
                    }
                } else {
                    let address = u16::from(self.next_byte()).wrapping_add(self.x as u16);
                    AddressingMode::Indirect(self.memory.read_double(address))
                }
            }
            1 => AddressingMode::Indirect(self.next_byte() as u16),
            2 => { 
                if c & 1 == 0 {
                    if (c & 0b10 != 0) && (a & 0b10 != 0) {
                        AddressingMode::Accumulator
                    } else {
                        AddressingMode::Implied
                    }
                } else {
                    AddressingMode::Immediate(self.next_byte())
                }
            }
            3 => AddressingMode::Indirect(u16::from(self.next_byte()) + u16::from(self.next_byte()) << 8),
            4 => if c & 1 == 0 {
                    let offset = self.next_byte() as i16;
                    if offset >= 0 {
                        AddressingMode::Indirect(self.program_counter + offset as u16)
                    } else {
                        AddressingMode::Indirect(self.program_counter - offset.unsigned_abs())
                    }
                } else {
                    let zero_page_address = self.next_byte();
                    let full_address = self.memory.read_double(zero_page_address as u16);
                    let value_at_full_address = self.memory.read_double(full_address);
                    AddressingMode::Indirect(value_at_full_address + self.y as u16)
                }
            5 => { 
                if (a == 4 || a == 5) && (c & 0b10 != 0) {
                    AddressingMode::Indirect(self.next_byte().wrapping_add(self.y) as u16)
                } else {
                    AddressingMode::Indirect(self.next_byte().wrapping_add(self.x) as u16)
                }
            }
            6 => if c & 1 == 0 {
                    AddressingMode::Implied
                } else {
                    AddressingMode::Indirect((self.y as u16).wrapping_add(self.next_byte() as u16).wrapping_add(u16::from(self.next_byte()) << 8))
                }
            7 => AddressingMode::Indirect((self.x as u16).wrapping_add(self.next_byte() as u16).wrapping_add(u16::from(self.next_byte()) << 8)),
            _ => unreachable!()
        }
    }

    fn value_of(&self, addressing_mode: AddressingMode) -> Option<u8> {
        match addressing_mode {
            AddressingMode::Implied => None,
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
        (self.read(address + 1) as u16) << 8 + self.read(address) as u16
    }

    fn write(&mut self, address: u16, value: u8) {
        if address < 0x2000 {
            self.ram[(address % 0x800) as usize] = value
        } else {
            todo!("Write > 0x2000")
        }
    }
}
