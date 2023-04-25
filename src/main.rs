use std::alloc::{self, Layout};
use bitflags::bitflags;

enum RegisterEncoding {
  AX = 0b000,
  CX = 0b001,
  DX = 0b010,
  BX = 0b011,
  SP = 0b100,
  BP = 0b101,
  SI = 0b110,
  DI = 0b111,
}

impl TryFrom<u8> for RegisterEncoding {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == RegisterEncoding::AX as u8 => Ok(RegisterEncoding::AX),
            x if x == RegisterEncoding::CX as u8 => Ok(RegisterEncoding::CX),
            x if x == RegisterEncoding::DX as u8 => Ok(RegisterEncoding::DX),
            x if x == RegisterEncoding::BX as u8 => Ok(RegisterEncoding::BX),
            x if x == RegisterEncoding::SP as u8 => Ok(RegisterEncoding::SP),
            x if x == RegisterEncoding::BP as u8 => Ok(RegisterEncoding::BP),
            x if x == RegisterEncoding::SI as u8 => Ok(RegisterEncoding::SI),
            x if x == RegisterEncoding::DI as u8 => Ok(RegisterEncoding::DI),
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

    fn mutate_register(&mut self, register: RegisterEncoding, mutation: fn(&mut u16) -> ()) {
        match register {
            RegisterEncoding::AX => mutation(&mut self.ax),
            RegisterEncoding::BX => mutation(&mut self.bx),
            RegisterEncoding::CX => mutation(&mut self.cx),
            RegisterEncoding::DX => mutation(&mut self.dx),
            RegisterEncoding::SI => mutation(&mut self.si),
            RegisterEncoding::DI => mutation(&mut self.di),
            RegisterEncoding::BP => mutation(&mut self.bp),
            RegisterEncoding::SP => mutation(&mut self.sp),
        }
    }
}

struct Emulator {
    cpu: CPU,
    ram: Box<[u8]>,
    disk: Box<[u8]>,
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
            self.execute().unwrap()
        }
    }

    fn execute(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let address = U20::new(self.cpu.cs, self.cpu.ip);
        if self.ram[address.0 as usize] == 0b11110100 {
            return Err("Halted".into());
        }
        match self.ram[address.0 as usize] >> 3 {
            0b01000 => self.inc(address),
            _ => println!("unimplemented instruction")
        }
        self.cpu.ip += 1;
        Ok(())
    }

    fn inc(&mut self, address: U20) {
        let register: RegisterEncoding = (self.ram[address.0 as usize] & 0b00000111).try_into().unwrap();
        self.cpu.mutate_register(register, |r: &mut u16| *r = *r + 1);
    }
}

struct U20(u32);

impl U20 {
    fn new(segment: u16, offset: u16) -> Self {
        U20(u32::from(segment) * 16 + u32::from(offset))
    }
}

enum Instruction {
    INC(RegisterEncoding),
}

fn main() {
    let mut disk = alloc_box_buffer(1024 * 1024 * 50);

    disk[510] = 0x55;
    disk[511] = 0xAA;

    disk[0] = 0b01000101; // inc bp
    disk[1] = 0xF4; // hlt

    let mut emulator = Emulator::new(disk);
    emulator.run().unwrap();

    assert_eq!(emulator.cpu.bp, 1);
}
