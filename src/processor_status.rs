#[derive(Clone, Copy)]
pub enum StatusBit {
    Carry = 0,
    Zero = 1,
    InterruptDisable = 2,
    DecimalMode = 3,
    Break = 4,
    Overflow = 6,
    Negative = 7,
}

pub struct StatusRegister {
    status: u8,
}

impl StatusRegister {
    pub fn new() -> Self {
        StatusRegister { status: 0b00100000}
    }

    pub fn set_flag(&mut self, bit: StatusBit, value: bool) {
        self.status |= (value as u8) << (bit as u8);
    }

    pub fn get_flag(&self, bit: StatusBit) -> bool {
        self.status & (1 << (bit as u8)) > 0
    }
    
    pub fn load(&mut self, value: u8) {
        self.status = value | 0b00100000
    }
}

impl From<&StatusRegister> for u8 {
    fn from(status: &StatusRegister) -> Self {
        status.status
    }
}
