use crate::Bus16;
use std::{cell::RefCell, rc::Rc};

pub struct OLC6502 {
    bus: Option<Rc<RefCell<Bus16>>>,

    acc: u8,
    x: u8,
    y: u8,
    stkp: u8,
    pc: u16,
    status: u8,

    fetched: u8,

    addr_abs: u16,
    addr_rel: u16,
    opcode: u8,
    cycles: u8,
}

pub struct Instruction {
    name: &'static str,
    operation: Box<dyn Fn() -> u8>,
    addrmode: Box<dyn Fn() -> u8>,
    cycles: u8,
}

pub struct FlagsOLC6502;

impl FlagsOLC6502 {
    const CARRY: u8 = 1 << 0;
    const ZERO: u8 = 1 << 1;
    const DISABLE_INTERRUPTS: u8 = 1 << 2;
    const DECIMAL_MODE: u8 = 1 << 3;
    const BREAK: u8 = 1 << 4;
    const UNUSED: u8 = 1 << 5;
    const OVERFLOW: u8 = 1 << 6;
    const NEGATIVE: u8 = 1 << 7;
}

impl OLC6502 {
    fn bus(&self) -> std::cell::RefMut<'_, Bus16> {
        self.bus
            .as_ref()
            .expect("Bus not connected to CPU!")
            .borrow_mut()
    }

    pub fn write(&self, addr: u16, data: u8) {
        self.bus().write(addr, data);
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.bus().read(addr)
    }

    pub fn get_flag(&self, flag: u8) -> u8 {
        todo!()
    }

    pub fn set_flag(&mut self, flag: u8, val: bool) {}
}

impl OLC6502 {
    pub fn clock(&mut self) {
        if self.cycles == 0 {
            let opcode = self.read(self.pc);
            self.pc += 1;

            let additional_cycles_required = self.execute(opcode);

            if additional_cycles_required {
                self.cycles += 1;
            }
        }

        self.cycles -= 1;
    }

    pub fn reset() {}
    pub fn irq() {}
    pub fn nmi() {}

    pub fn fetch() -> u8 {
        0
    }
}

impl OLC6502 {
    pub fn execute(&mut self, opcode: u8) -> bool {
        true
    }

    // pub fn lookup() -> [Instruction; 3] {
    //     [
    // Instruction {
    //     name: "brk",
    //     operation: Box::new(Self::brk),
    //     addrmode: Box::new(Self::imm),
    //     cycles: 7,
    // },
    //         Instruction {
    //             name: todo!(),
    //             operation: todo!(),
    //             addrmode: todo!(),
    //             cycles: todo!(),
    //         },
    //         Instruction {
    //             name: todo!(),
    //             operation: todo!(),
    //             addrmode: todo!(),
    //             cycles: todo!(),
    //         },
    //     ]
    // }
}

/// Addressing Modes
impl OLC6502 {
    pub fn imp() -> u8 {
        0
    }
    pub fn imm() -> u8 {
        0
    }
    pub fn zp0() -> u8 {
        0
    }
    pub fn zpx() -> u8 {
        0
    }
    pub fn zpy() -> u8 {
        0
    }
    pub fn rel() -> u8 {
        0
    }
    pub fn abs() -> u8 {
        0
    }
    pub fn abx() -> u8 {
        0
    }
    pub fn aby() -> u8 {
        0
    }
    pub fn ind() -> u8 {
        0
    }
    pub fn izx() -> u8 {
        0
    }
    pub fn izy() -> u8 {
        0
    }
}

/// OP codes
impl OLC6502 {
    pub fn adc() -> u8 {
        0
    }

    pub fn and() -> u8 {
        0
    }

    pub fn asl() -> u8 {
        0
    }

    pub fn bcc() -> u8 {
        0
    }
    pub fn bcs() -> u8 {
        0
    }

    pub fn beq() -> u8 {
        0
    }

    pub fn bit() -> u8 {
        0
    }

    pub fn bmi() -> u8 {
        0
    }

    pub fn bne() -> u8 {
        0
    }

    pub fn bpl() -> u8 {
        0
    }

    pub fn brk() -> u8 {
        0
    }

    pub fn bvc() -> u8 {
        0
    }
    pub fn bvs() -> u8 {
        0
    }

    pub fn clc() -> u8 {
        0
    }
    pub fn cld() -> u8 {
        0
    }
    pub fn cli() -> u8 {
        0
    }
    pub fn clv() -> u8 {
        0
    }

    pub fn cmp() -> u8 {
        0
    }

    pub fn cpx() -> u8 {
        0
    }
    pub fn cpy() -> u8 {
        0
    }

    pub fn dec() -> u8 {
        0
    }
    pub fn dex() -> u8 {
        0
    }
    pub fn dey() -> u8 {
        0
    }

    pub fn eor() -> u8 {
        0
    }

    pub fn inc() -> u8 {
        0
    }
    pub fn inx() -> u8 {
        0
    }
    pub fn iny() -> u8 {
        0
    }

    pub fn jmp() -> u8 {
        0
    }

    pub fn jsr() -> u8 {
        0
    }

    pub fn lda() -> u8 {
        0
    }
    pub fn ldx() -> u8 {
        0
    }
    pub fn ldy() -> u8 {
        0
    }

    pub fn lsr() -> u8 {
        0
    }

    pub fn nop() -> u8 {
        0
    }

    pub fn ora() -> u8 {
        0
    }

    pub fn pha() -> u8 {
        0
    }
    pub fn php() -> u8 {
        0
    }

    pub fn pla() -> u8 {
        0
    }
    pub fn plp() -> u8 {
        0
    }

    pub fn rol() -> u8 {
        0
    }
    pub fn ror() -> u8 {
        0
    }

    pub fn rti() -> u8 {
        0
    }
    pub fn rts() -> u8 {
        0
    }

    pub fn sbc() -> u8 {
        0
    }

    pub fn sec() -> u8 {
        0
    }
    pub fn sed() -> u8 {
        0
    }
    pub fn sei() -> u8 {
        0
    }

    pub fn sta() -> u8 {
        0
    }
    pub fn stx() -> u8 {
        0
    }
    pub fn sty() -> u8 {
        0
    }

    pub fn tax() -> u8 {
        0
    }
    pub fn tay() -> u8 {
        0
    }
    pub fn tsx() -> u8 {
        0
    }
    pub fn txa() -> u8 {
        0
    }
    pub fn txs() -> u8 {
        0
    }
    pub fn tya() -> u8 {
        0
    }

    pub fn xxx() -> u8 {
        0
    }
}
