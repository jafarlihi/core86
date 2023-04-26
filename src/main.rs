use std::alloc::{self, Layout};
use bitflags::bitflags;

enum RegisterEncoding {
    RegisterEncoding8(RegisterEncoding8),
    RegisterEncoding16(RegisterEncoding16),
}

enum RM {
    Register(RegisterEncoding),
    BaseIndex((Option<RegisterEncoding16>, Option<RegisterEncoding16>)),
}

enum RegisterEncoding16 {
    AX = 0b000,
    CX = 0b001,
    DX = 0b010,
    BX = 0b011,
    SP = 0b100,
    BP = 0b101,
    SI = 0b110,
    DI = 0b111,
}

impl TryFrom<u8> for RegisterEncoding16 {
    type Error = ();
    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == RegisterEncoding16::AX as u8 => Ok(RegisterEncoding16::AX),
            x if x == RegisterEncoding16::CX as u8 => Ok(RegisterEncoding16::CX),
            x if x == RegisterEncoding16::DX as u8 => Ok(RegisterEncoding16::DX),
            x if x == RegisterEncoding16::BX as u8 => Ok(RegisterEncoding16::BX),
            x if x == RegisterEncoding16::SP as u8 => Ok(RegisterEncoding16::SP),
            x if x == RegisterEncoding16::BP as u8 => Ok(RegisterEncoding16::BP),
            x if x == RegisterEncoding16::SI as u8 => Ok(RegisterEncoding16::SI),
            x if x == RegisterEncoding16::DI as u8 => Ok(RegisterEncoding16::DI),
            _ => Err(()),
        }
    }
}

enum RegisterEncoding8 {
    AL = 0b000,
    CL = 0b001,
    DL = 0b010,
    BL = 0b011,
    AH = 0b100,
    CH = 0b101,
    DH = 0b110,
    BH = 0b111,
}

impl TryFrom<u8> for RegisterEncoding8 {
    type Error = ();
    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == RegisterEncoding8::AL as u8 => Ok(RegisterEncoding8::AL),
            x if x == RegisterEncoding8::CL as u8 => Ok(RegisterEncoding8::CL),
            x if x == RegisterEncoding8::DL as u8 => Ok(RegisterEncoding8::DL),
            x if x == RegisterEncoding8::BL as u8 => Ok(RegisterEncoding8::BL),
            x if x == RegisterEncoding8::AH as u8 => Ok(RegisterEncoding8::AH),
            x if x == RegisterEncoding8::CH as u8 => Ok(RegisterEncoding8::CH),
            x if x == RegisterEncoding8::DH as u8 => Ok(RegisterEncoding8::DH),
            x if x == RegisterEncoding8::BH as u8 => Ok(RegisterEncoding8::BH),
            _ => Err(()),
        }
    }
}

fn alloc_box_buffer(len: usize) -> Box<[u8]> {
    if len == 0 {
        return <Box<[u8]>>::default();
    }
    let layout = Layout::array::<u8>(len).unwrap();
    let ptr = unsafe { alloc::alloc_zeroed(layout) };
    let slice_ptr = core::ptr::slice_from_raw_parts_mut(ptr, len);
    unsafe { Box::from_raw(slice_ptr) }
}

bitflags! {
    struct Flags: u16 {
        const CF = 0b0000000000000001;
        const PF = 0b0000000000000100;
        const AF = 0b0000000000010000;
        const ZF = 0b0000000001000000;
        const SF = 0b0000000010000000;
        const TF = 0b0000000100000000;
        const IF = 0b0000001000000000;
        const DF = 0b0000010000000000;
        const OF = 0b0000100000000000;
    }
}

struct CPU {
    ax: u16,
    bx: u16,
    cx: u16,
    dx: u16,
    si: u16,
    di: u16,
    bp: u16,
    sp: u16,
    ip: u16,
    cs: u16,
    ds: u16,
    es: u16,
    ss: u16,
    flags: Flags,
}

enum RegisterHalf {
    FULL,
    HIGH,
    LOW,
}

impl CPU {
    fn new() -> Self {
        CPU {
            ax: 0,
            bx: 0,
            cx: 0,
            dx: 0,
            si: 0,
            di: 0,
            bp: 0,
            sp: 0,
            ip: 0,
            cs: 0xFFFF,
            ds: 0,
            es: 0,
            ss: 0,
            flags: Flags::empty(),
        }
    }

    fn mutate_register(&mut self, register: RegisterEncoding, mutation: fn(&mut u16, RegisterHalf) -> ()) {
        match register {
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX) => mutation(&mut self.ax, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::BX) => mutation(&mut self.bx, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::CX) => mutation(&mut self.cx, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX) => mutation(&mut self.dx, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::SI) => mutation(&mut self.si, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DI) => mutation(&mut self.di, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::BP) => mutation(&mut self.bp, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::SP) => mutation(&mut self.sp, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH) => mutation(&mut self.ax, RegisterHalf::HIGH),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL) => mutation(&mut self.ax, RegisterHalf::LOW),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::BH) => mutation(&mut self.bx, RegisterHalf::HIGH),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::BL) => mutation(&mut self.bx, RegisterHalf::LOW),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CH) => mutation(&mut self.cx, RegisterHalf::HIGH),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CL) => mutation(&mut self.cx, RegisterHalf::LOW),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::DH) => mutation(&mut self.dx, RegisterHalf::HIGH),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::DL) => mutation(&mut self.dx, RegisterHalf::LOW),
        }
    }
}

struct Emulator {
    cpu: CPU,
    ram: Box<[u8]>,
    disk: Box<[u8]>,
}

enum ModRMMod {
    NoDisplacement = 0b00,
    OneByteDisplacement = 0b01,
    TwoByteDisplacement = 0b10,
    Register = 0b11,
}

impl TryFrom<u8> for ModRMMod {
    type Error = ();
    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == ModRMMod::NoDisplacement as u8 => Ok(ModRMMod::NoDisplacement),
            x if x == ModRMMod::OneByteDisplacement as u8 => Ok(ModRMMod::OneByteDisplacement),
            x if x == ModRMMod::TwoByteDisplacement as u8 => Ok(ModRMMod::TwoByteDisplacement),
            x if x == ModRMMod::Register as u8 => Ok(ModRMMod::Register),
            _ => Err(()),
        }
    }
}

impl Emulator {
    fn new(disk: Box<[u8]>) -> Self {
        Emulator {
            cpu: CPU::new(),
            ram: alloc_box_buffer(1024 * 1024),
            disk,
        }
    }

    fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        if self.disk[510] != 0x55 && self.disk[511] != 0xAA {
            return Err("Disk not bootable".into());
        }
        self.ram[0x7c00..0x7c00 + 512].copy_from_slice(&self.disk[..512]);
        self.cpu.cs = 0x0000;
        self.cpu.ip = 0x7c00;
        loop {
            match self.execute() {
                Ok(()) => (),
                Err(error) => return Err(error),
            }
        }
    }

    fn parse_modrm(w: bool, modrm: &u8) -> (ModRMMod, u8, RM) {
        let m: ModRMMod = (modrm >> 6).try_into().unwrap();
        let opcode = (modrm & 0b00111000) >> 3;
        let rm = match m {
            ModRMMod::Register => {
                if w {
                    RM::Register(RegisterEncoding::RegisterEncoding16(RegisterEncoding16::try_from(modrm & 0b00000111).unwrap()))
                } else {
                    RM::Register(RegisterEncoding::RegisterEncoding8(RegisterEncoding8::try_from(modrm & 0b00000111).unwrap()))
                }
            }
            _ => {
                match modrm & 0b00000111 {
                    0b00000000 => RM::BaseIndex((Some(RegisterEncoding16::BX), Some(RegisterEncoding16::SI))),
                    0b00000001 => RM::BaseIndex((Some(RegisterEncoding16::BX), Some(RegisterEncoding16::DI))),
                    0b00000010 => RM::BaseIndex((Some(RegisterEncoding16::BP), Some(RegisterEncoding16::SI))),
                    0b00000011 => RM::BaseIndex((Some(RegisterEncoding16::BP), Some(RegisterEncoding16::DI))),
                    0b00000100 => RM::BaseIndex((None, Some(RegisterEncoding16::SI))),
                    0b00000101 => RM::BaseIndex((None, Some(RegisterEncoding16::DI))),
                    0b00000110 => RM::BaseIndex((Some(RegisterEncoding16::BP), None)),
                    0b00000111 => RM::BaseIndex((Some(RegisterEncoding16::BX), None)),
                    _ => panic!("This should never happen")
                }
            }
        };
        (m, opcode, rm)
    }

    fn execute(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let address = U20::new(self.cpu.cs, self.cpu.ip);
        if self.ram[address.0 as usize] == 0b11110100 {
            return Err("Halted".into());
        }
        let mut instruction_size = 1;
        match self.ram[address.0 as usize] >> 3 {
            0b01000 => {
                let register = RegisterEncoding::RegisterEncoding16((self.ram[address.0 as usize] & 0b00000111).try_into().unwrap());
                self.inc_register(register)
            },
            _ => (),
        }
        match self.ram[address.0 as usize] >> 1 {
            0b1111111 => {
                instruction_size += 1;
                let w = self.ram[address.0 as usize] & 0b00000001 != 0;
                let modrm = Self::parse_modrm(w, &self.ram[address.0 as usize + 1]);
                match modrm.1 {
                    0b00000000 => {
                        match modrm.0 {
                            ModRMMod::Register => {
                                self.inc(modrm.2)
                            },
                            _ => (),
                        }
                    },
                    _ => (),
                }
            },
            _ => (),
        }
        self.cpu.ip += instruction_size;
        Ok(())
    }

    fn inc(&mut self, rm: RM) {
        match rm {
            RM::Register(register_encoding) => self.inc_register(register_encoding),
            _ => (),
        }
    }

    fn inc_register(&mut self, register: RegisterEncoding) {
        self.cpu.mutate_register(register, |r: &mut u16, h: RegisterHalf| {
            match h {
                RegisterHalf::FULL => *r = *r + 1,
                RegisterHalf::HIGH => {
                    let mut high = *r >> 8;
                    high += 1;
                    high = high << 8 & 0b1111111100000000;
                    let low = *r & 0b0000000011111111;
                    *r = high | low;
                },
                RegisterHalf::LOW => {
                    let mut low = *r & 0b0000000011111111;
                    low += 1;
                    low = low & 0b0000000011111111;
                    let high = *r & 0b1111111100000000;
                    *r = high | low;
                },
            }
        });
    }
}

struct U20(u32);

impl U20 {
    fn new(segment: u16, offset: u16) -> Self {
        U20(u32::from(segment) * 16 + u32::from(offset))
    }
}

fn main() {
    let mut disk = alloc_box_buffer(1024 * 1024 * 50);

    disk[510] = 0x55;
    disk[511] = 0xAA;

    // inc bp
    disk[0] = 0b01000101;
    // inc cx (with modrm)
    disk[1] = 0b11111111;
    disk[2] = 0b11000001;
    // inc bl
    disk[3] = 0b11111110;
    disk[4] = 0b11000011;
    // hlt
    disk[5] = 0xF4;

    let mut emulator = Emulator::new(disk);
    emulator.cpu.bx = 0b0000000011111111;
    let run = emulator.run();

    match run {
        Ok(()) => (),
        Err(_error) => {
            assert_eq!(emulator.cpu.bp, 1);
            assert_eq!(emulator.cpu.cx, 1);
            assert_eq!(emulator.cpu.bx, 0);
        }
    }
}
