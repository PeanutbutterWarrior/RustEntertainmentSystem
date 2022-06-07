use crate::processor_status;
use crate::processor_status::StatusBit;
use crate::memory;

#[derive(Clone, Copy)]
enum AddressingMode {
    Implied,
    Accumulator,
    Immediate(u8),
    Indirect(u16),
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Interrupt {
    Reset,
    NonMaskableInterrupt,
    InterruptRequest,
}

impl Interrupt {
    fn higher_priority(a: Self, b: Self) -> Self {
        if a == Interrupt::Reset {
            a
        } else if a == Interrupt:: InterruptRequest || b == Interrupt::Reset {
            b
        } else {
            a
        }
    }
}

pub struct CPU {
    program_counter: u16,
    stack_pointer: u8,
    accumulator: u8,
    x: u8,
    y: u8,
    status: processor_status::StatusRegister,
    memory: memory::Memory,
    interrupt: Option<Interrupt>,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            program_counter: 0,
            stack_pointer: u8::MAX,
            accumulator: 0,
            x: 0,
            y: 0,
            status: processor_status::StatusRegister::new(),
            memory: memory::Memory::from_cartridge("roms/nestest.nes"), // TODO make better
            interrupt: Some(Interrupt::Reset),
        }
    }

    pub fn execute_next_instruction(&mut self) {
        let opcode = self.next_byte();
        let operand = self.get_addressing_mode(opcode);
        
        if opcode == 0x00 { // BRK
            match self.interrupt {
                Some(current_interrupt) => self.interrupt = Some(Interrupt::higher_priority(current_interrupt, Interrupt::InterruptRequest)),
                None => self.interrupt = Some(Interrupt::InterruptRequest),
            }
            self.status.set_flag(StatusBit::Break, true);

        } else if opcode == 0x08 { // PHP
            self.push_stack(u8::from(&self.status) | 0b00110000);

        } else if opcode == 0x10 { // BPL
            if !self.status.get_flag(StatusBit::Negative) {
                if let AddressingMode::Indirect(address) = operand {
                    self.program_counter = address;
                }
            }
        
        } else if opcode == 0x18 { // CLC
            self.status.set_flag(StatusBit::Carry, false)

        } else if opcode == 0x20 { // JSR
            if let AddressingMode::Indirect(address) = operand {
                let [lo, hi] = self.program_counter.to_le_bytes();
                self.push_stack(hi);
                self.push_stack(lo);
                self.program_counter = address;
            }
        
        } else if opcode == 0x28 { // PLP
            let status = self.pop_stack();
            self.status.load(status);

        } else if opcode == 0x30 { // BMI
            if self.status.get_flag(StatusBit::Negative) {
                if let AddressingMode::Indirect(address) = operand {
                    self.program_counter = address;
                }
            }
        
        } else if opcode == 0x38 { // SEC
            self.status.set_flag(StatusBit::Carry, true);

        } else if opcode == 0x40 { // RTI
            let status = self.pop_stack();
            self.status.load(status);
            self.program_counter = self.pop_stack() as u16 + ((self.pop_stack() as u16) << 8);
        
        } else if opcode == 0x48 { // PHA
            self.push_stack(self.accumulator);
        } else if opcode == 0x50 { // BVC
            if !self.status.get_flag(StatusBit::Overflow) {
                if let AddressingMode::Indirect(address) = operand {
                    self.program_counter = address;
                }
            }
        
        } else if opcode == 0x58 { // CLI
            self.status.set_flag(StatusBit::InterruptDisable, false);
        } else if opcode == 0x60 { // RTS
            self.program_counter = self.pop_stack() as u16 + ((self.pop_stack() as u16) << 8);

        } else if opcode == 0x68 { // PLA
            self.accumulator = self.pop_stack();
        } else if opcode == 0x70 { // BVS
            if self.status.get_flag(StatusBit::Overflow) {
                if let AddressingMode::Indirect(address) = operand {
                    self.program_counter = address;
                }
            }
        
        } else if opcode == 0x78 { // SEI
            self.status.set_flag(StatusBit::InterruptDisable, true);

        } else if opcode == 0x88 { // DEY
            self.y = self.y.wrapping_sub(1);
            self.status.set_flag(StatusBit::Negative, self.y > 127);
            self.status.set_flag(StatusBit::Zero, self.y == 0);

        } else if opcode == 0x8A { // TXA
            self.accumulator = self.x;
            self.status.set_flag(StatusBit::Negative, self.accumulator > 127);
            self.status.set_flag(StatusBit::Zero, self.accumulator == 0);
        
        } else if opcode == 0x90 { // BCC
            if !self.status.get_flag(StatusBit::Carry) {
                if let AddressingMode::Indirect(address) = operand {
                    self.program_counter = address;
                }
            }
        
        } else if opcode == 0x98 { // TYA
            self.accumulator = self.y;
            self.status.set_flag(StatusBit::Negative, self.accumulator > 127);
            self.status.set_flag(StatusBit::Zero, self.accumulator == 0);

        } else if opcode == 0x9A { // TXS
            self.stack_pointer = self.x;

        } else if opcode == 0xA8 { // TAY
            self.y = self.accumulator;
            self.status.set_flag(StatusBit::Negative, self.y > 127);
            self.status.set_flag(StatusBit::Zero, self.y == 0);

        } else if opcode == 0xAA { // TAX
            self.x = self.accumulator;
            self.status.set_flag(StatusBit::Negative, self.x > 127);
            self.status.set_flag(StatusBit::Zero, self.x == 0);

        } else if opcode == 0xB0 { // BCS
            if self.status.get_flag(StatusBit::Carry) {
                if let AddressingMode::Indirect(address) = operand {
                    self.program_counter = address;
                }
            }
        
        } else if opcode == 0xB8 { // CLV
            self.status.set_flag(StatusBit::Overflow, false);

        } else if opcode == 0xBA { // TSX
            self.x = self.stack_pointer;
            self.status.set_flag(StatusBit::Negative, self.x > 127);
            self.status.set_flag(StatusBit::Zero, self.x == 0);

        } else if opcode == 0xC8 { // INY
            self.y = self.y.wrapping_add(1);
            self.status.set_flag(StatusBit::Negative, self.y > 127);
            self.status.set_flag(StatusBit::Zero, self.y == 0);

        } else if opcode == 0xCA { // DEX
            self.x = self.x.wrapping_sub(1);
            self.status.set_flag(StatusBit::Negative, self.x > 127);
            self.status.set_flag(StatusBit::Zero, self.x == 0);
        
        } else if opcode == 0xD0 { // BNE
            if !self.status.get_flag(StatusBit::Zero) {
                if let AddressingMode::Indirect(address) = operand {
                    self.program_counter = address;
                }
            }
        
        } else if opcode == 0xD8 { // CLD
            self.status.set_flag(StatusBit::DecimalMode, false);

        } else if opcode == 0xE8 { // INX
            self.x = self.x.wrapping_add(1);
            self.status.set_flag(StatusBit::Negative, self.x > 127);
            self.status.set_flag(StatusBit::Zero, self.x == 0);

        } else if opcode == 0xEA { // NOP

        } else if opcode == 0xF0 { // BEQ
            if self.status.get_flag(StatusBit::Zero) {
                if let AddressingMode::Indirect(address) = operand {
                    self.program_counter = address;
                }
            }
        
        } else if opcode == 0xF8 { // SED
            self.status.set_flag(StatusBit::DecimalMode, true);

        } else if opcode == 0x4C || opcode == 0x6C { // JSR
            if let AddressingMode::Indirect(address) = operand {
                self.program_counter = address
            }

        } else if opcode == 0x24 || opcode == 0x2C { // BIT
            let value = self.value_of(operand).unwrap();
            self.status.set_flag(StatusBit::Negative, value & 0x80 > 0);
            self.status.set_flag(StatusBit::Overflow, value & 0x40 > 0);
            self.status.set_flag(StatusBit::Zero, value & self.accumulator > 0);
        } else if opcode & 0b111_000_11 == 0b000_000_01 { // ORA
            self.accumulator |= self.value_of(operand).unwrap();
            self.status.set_flag(StatusBit::Negative, self.accumulator > 127);
            self.status.set_flag(StatusBit::Zero, self.accumulator == 0);

        } else if opcode & 0b111_000_11 == 0b001_000_01 { // AND
            self.accumulator &= self.value_of(operand).unwrap();
            self.status.set_flag(StatusBit::Negative, self.accumulator > 127);
            self.status.set_flag(StatusBit::Zero, self.accumulator == 0);
        
        } else if opcode & 0b111_000_11 == 0b010_000_01 { // EOR
            self.accumulator ^= self.value_of(operand).unwrap();
            self.status.set_flag(StatusBit::Negative, self.accumulator > 127);
            self.status.set_flag(StatusBit::Zero, self.accumulator == 0);

        } else if opcode & 0b111_000_11 == 0b011_000_01 { // ADC
            let value = self.value_of(operand).unwrap();
            let (mut result, mut carry) = self.accumulator.overflowing_add(value);
            let mut overflow = (self.accumulator ^ result) & (value ^ result) & 0x80 != 0; // From http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
            
            if self.status.get_flag(StatusBit::Carry) {
                result = result.wrapping_add(1);
                if result == 0 {
                    carry = true;
                    overflow = true;
                } else if result == 128 {
                    overflow = true;
                }
            }
            self.accumulator = result;
            self.status.set_flag(StatusBit::Carry, carry);
            self.status.set_flag(StatusBit::Overflow, overflow);
            self.status.set_flag(StatusBit::Negative, self.accumulator > 127);
            self.status.set_flag(StatusBit::Zero, self.accumulator == 0);
        
        } else if opcode & 0b111_000_11 == 0b100_000_01 { // STA
            if let AddressingMode::Indirect(addr) = operand {
                self.memory.write(addr, self.accumulator)
            }

        } else if opcode & 0b111_000_11 == 0b101_000_01 { // LDA
            self.accumulator = self.value_of(operand).unwrap();
            self.status.set_flag(StatusBit::Negative, self.accumulator > 127);
            self.status.set_flag(StatusBit::Zero, self.accumulator == 0);
        
        } else if opcode & 0b111_000_11 == 0b110_000_01 { // CMP
            let (result, carry) = self.value_of(operand).unwrap().overflowing_sub(self.accumulator);
            self.status.set_flag(StatusBit::Carry, carry);
            self.status.set_flag(StatusBit::Negative, result > 127);
            self.status.set_flag(StatusBit::Zero, result == 0);
        } else if opcode & 0b111_000_11 == 0b111_000_01 { // SBC
            let value = !self.value_of(operand).unwrap();
            let (mut result, mut carry) = self.accumulator.overflowing_add(value);
            let mut overflow = (self.accumulator ^ result) & (value ^ result) & 0x80 != 0; // From http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
            
            if self.status.get_flag(StatusBit::Carry) {
                result = result.wrapping_add(1);
                if result == 0 {
                    carry = true;
                    overflow = true;
                } else if result == 128 {
                    overflow = true;
                }
            }
            self.accumulator = result;
            self.status.set_flag(StatusBit::Carry, carry);
            self.status.set_flag(StatusBit::Overflow, overflow);
            self.status.set_flag(StatusBit::Negative, self.accumulator > 127);
            self.status.set_flag(StatusBit::Zero, self.accumulator == 0);
        } else if opcode & 0b111_000_11 == 0b000_000_10 { // ASL
            if opcode == 0x02 || opcode == 0x12 {
                self.halt();
            } else if opcode != 0x1A {
                let value = self.value_of(operand).unwrap();
                let carry = value & 0x80 != 0;
                let result = value << 1;
                self.write_to_addressing_mode(operand, result);
                self.status.set_flag(StatusBit::Carry, carry);
                self.status.set_flag(StatusBit::Negative, result > 127);
                self.status.set_flag(StatusBit::Zero, result == 0);
            }
        
        } else if opcode & 0b111_000_11 == 0b001_000_10 { // ROL
            if opcode == 0x22 || opcode == 0x32 {
                self.halt();
            } else if opcode != 0x3A {
                let value = self.value_of(operand).unwrap();
                let carry = value & 0x80 != 0;
                let result = (value << 1) | self.status.get_flag(StatusBit::Carry) as u8;
                self.write_to_addressing_mode(operand, result);
                self.status.set_flag(StatusBit::Carry, carry);
                self.status.set_flag(StatusBit::Negative, result > 127);
                self.status.set_flag(StatusBit::Zero, result == 0);
            }
        
        } else if opcode & 0b111_000_11 == 0b010_000_10 { // LSR
            if opcode == 0x42 || opcode == 0x52 {
                self.halt();
            } else if opcode != 0x5A {
                let value = self.value_of(operand).unwrap();
                let carry = value & 0x01 != 0;
                let result = value >> 1;
                self.write_to_addressing_mode(operand, result);
                self.status.set_flag(StatusBit::Carry, carry);
                self.status.set_flag(StatusBit::Negative, result > 127);
                self.status.set_flag(StatusBit::Zero, result == 0);
            }
        } else if opcode & 0b111_000_11 == 0b011_000_10 { // ROR
            if opcode == 0x62 || opcode == 0x72 {
                self.halt();
            } else if opcode != 0x7A {
                let value = self.value_of(operand).unwrap();
                let carry = value & 0x01 != 0;
                let result = (value >> 1) | ((self.status.get_flag(StatusBit::Carry) as u8) << 7);
                self.write_to_addressing_mode(operand, result);
                self.status.set_flag(StatusBit::Carry, carry);
                self.status.set_flag(StatusBit::Negative, result > 127);
                self.status.set_flag(StatusBit::Zero, result == 0);
            }
        
        } else if opcode & 0b111_000_11 == 0b100_000_10 { // STX
            if let AddressingMode::Indirect(address) = operand {
                self.memory.write(address, self.x);
            }
        
        } else if opcode & 0b111_000_11 == 0b101_000_10 { // LDX
            self.x = self.value_of(operand).unwrap();
            self.status.set_flag(StatusBit::Negative, self.x > 127);
            self.status.set_flag(StatusBit::Zero, self.x == 0);

        } else if opcode & 0b111_000_11 == 0b110_000_10 { // DEC
            if let AddressingMode::Indirect(address) = operand {
                let result = self.memory.read(address).wrapping_sub(1);
                self.memory.write(address, result);
                self.status.set_flag(StatusBit::Negative, result > 127);
                self.status.set_flag(StatusBit::Zero, result == 0);
            }
        
        } else if opcode & 0b111_000_11 == 0b111_000_10 { // INC
            if let AddressingMode::Indirect(address) = operand {
                let result = self.memory.read(address).wrapping_add(1);
                self.memory.write(address, result);
                self.status.set_flag(StatusBit::Negative, result > 127);
                self.status.set_flag(StatusBit::Zero, result == 0);
            }
        } else if opcode & 0b111_000_11 == 0b100_000_00 { // STY
            if let AddressingMode::Indirect(address) = operand {
                self.memory.write(address, self.y);
            }

        } else if opcode & 0b111_000_11 == 0b101_000_00 { // LDY
            self.y = self.value_of(operand).unwrap();
            self.status.set_flag(StatusBit::Negative, self.y > 127);
            self.status.set_flag(StatusBit::Zero, self.y == 0);
        } else if opcode & 0b111_000_11 == 0b110_000_00 { // CPY
            let (result, carry) = self.value_of(operand).unwrap().overflowing_sub(self.y);
            self.status.set_flag(StatusBit::Carry, carry);
            self.status.set_flag(StatusBit::Negative, result > 127);
            self.status.set_flag(StatusBit::Zero, result == 0);
        } else if opcode & 0b111_000_11 == 0b111_000_00 { // CPX
            let (result, carry) = self.value_of(operand).unwrap().overflowing_sub(self.x);
            self.status.set_flag(StatusBit::Carry, carry);
            self.status.set_flag(StatusBit::Negative, result > 127);
            self.status.set_flag(StatusBit::Zero, result == 0);
        }
    }

    pub fn run(&mut self) {
        loop {
            if let Some(interrupt) = &self.interrupt {
                let jump_vector = match interrupt {
                    Interrupt::Reset => 0xFFFA,
                    Interrupt::InterruptRequest => 0xFFFC,
                    Interrupt::NonMaskableInterrupt => 0xFFFE,
                };
                let [hi, low] = self.program_counter.to_be_bytes();
                self.push_stack(hi);
                self.push_stack(low);
                self.push_stack(u8::from(&self.status));
                self.program_counter = self.memory.read_double(jump_vector);
                
            }
            self.execute_next_instruction();
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
            3 => {
                let address = u16::from(self.next_byte()) + (u16::from(self.next_byte()) << 8);
                if opcode == 0x6C {
                    AddressingMode::Indirect(self.memory.read_double(address))
                } else {
                    AddressingMode::Indirect(address)
                }
            }
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

    fn write_to_addressing_mode(&mut self, addressing_mode: AddressingMode, value: u8) {
        match addressing_mode {
            AddressingMode::Accumulator => self.accumulator = value,
            AddressingMode::Indirect(address) => self.memory.write(address, value),
            _ => panic!("Tried writing to bad adressing mode"),
        }
    }

    fn halt(&self) {
        panic!("Halted")
    }

    fn push_stack(&mut self, value: u8) {
        self.memory.write(0x100 + self.stack_pointer as u16, value);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.memory.read(0x100 + self.stack_pointer as u16)
    }

}
