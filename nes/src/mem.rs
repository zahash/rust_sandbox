pub struct Mem64K {
    mem: [u8; Self::MEM_SIZE as usize],
}

impl Mem64K {
    const MEM_SIZE: u16 = u16::MAX;

    pub fn write(&mut self, addr: u16, data: u8) {
        self.mem[addr as usize] = data;
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }
}

impl Default for Mem64K {
    fn default() -> Self {
        Self {
            mem: [0; Self::MEM_SIZE as usize],
        }
    }
}
