use crate::nes::{Mem64K, OLC6502};

pub struct Bus16 {
    cpu: OLC6502,
    ram: Mem64K,
}

impl Bus16 {
    pub fn write(&mut self, addr: u16, data: u8) {
        self.ram.write(addr, data);
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.ram.read(addr)
    }
}
