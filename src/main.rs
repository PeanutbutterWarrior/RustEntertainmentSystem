mod nes;
mod processor_status;
mod memory;

fn main() {
    let mut cpu = nes::CPU::new();
    cpu.execute_next_instruction();
}