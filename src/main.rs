mod nes;
mod processor_status;

fn main() {
    let mut cpu = nes::CPU::new();
    cpu.execute_next_instruction();
}