pub struct Bus<'bus> {
    cpu: OLC6502<'bus>,
    ram: Mem64K,
}

#[derive(Default)]
pub struct OLC6502<'bus> {
    bus: Option<&'bus mut Bus<'bus>>,
}

pub struct Mem64K {
    mem: [u8; Self::MEM_SIZE as usize],
}

impl Bus<'_> {
    pub fn write(&mut self, addr: u16, data: u8) {
        self.ram.write(addr, data);
    }
    pub fn read(&self, addr: u16) -> u8 {
        self.ram.read(addr)
    }
}

impl<'bus> Bus<'bus> {
    pub fn init() -> Bus<'bus> {
        let mut bus = Self {
            cpu: OLC6502::default(),
            ram: Mem64K::default(),
        };

        let cpu = &mut bus.cpu;

        cpu.connect_bus(&mut bus);

        bus
    }
}

impl OLC6502<'_> {}

impl<'bus> OLC6502<'bus> {
    pub fn connect_bus(&mut self, bus: &'bus mut Bus<'bus>) {
        self.bus = Some(bus);
    }

    pub fn bus_mut(&mut self) -> &mut Bus<'bus> {
        self.bus.as_mut().expect("Bus not connected to CPU!")
    }

    pub fn bus(&self) -> &Bus<'bus> {
        self.bus.as_ref().expect("Bus not connected to CPU!")
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        self.bus_mut().write(addr, data);
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.bus().read(addr)
    }
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
