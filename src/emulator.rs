#![allow(arithmetic_overflow)]

use std::convert::TryFrom;
use std::ops::Neg;
use std::collections::VecDeque;
use bitflags::bitflags;
use num_enum::TryFromPrimitive;
use intbits::Bits;
use serde::Serialize;
use crate::video::update_mda_screen;
use crate::bios::{execute_isr, init_rom_configuration, init_ivt, load_bootsector, BIOS_SEG};

#[derive(Debug, Copy, Clone)]
pub enum RegisterEncoding {
    RegisterEncoding8(RegisterEncoding8),
    RegisterEncoding16(RegisterEncoding16),
}

#[derive(Copy, Clone)]
enum RM {
    Register(RegisterEncoding),
    BaseIndex((Option<RegisterEncoding16>, Option<RegisterEncoding16>)),
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Copy, Clone)]
#[repr(u8)]
pub enum RegisterEncoding16 {
    AX = 0b000,
    CX = 0b001,
    DX = 0b010,
    BX = 0b011,
    SP = 0b100,
    BP = 0b101,
    SI = 0b110,
    DI = 0b111,
    ES = 0xFA,
    CS = 0xFB,
    SS = 0xFC,
    DS = 0xFD,
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Copy, Clone)]
#[repr(u8)]
pub enum RegisterEncoding8 {
    AL = 0b000,
    CL = 0b001,
    DL = 0b010,
    BL = 0b011,
    AH = 0b100,
    CH = 0b101,
    DH = 0b110,
    BH = 0b111,
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct Flags: u16 {
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

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
enum SegmentRegister {
    ES = 0b00,
    CS = 0b01,
    SS = 0b10,
    DS = 0b11,
}

impl TryFrom<SegmentRegister> for RegisterEncoding16 {
    type Error = ();
    fn try_from(v: SegmentRegister) -> Result<Self, Self::Error> {
        match v {
            x if x == SegmentRegister::ES => Ok(RegisterEncoding16::ES),
            x if x == SegmentRegister::CS => Ok(RegisterEncoding16::CS),
            x if x == SegmentRegister::SS => Ok(RegisterEncoding16::SS),
            x if x == SegmentRegister::DS => Ok(RegisterEncoding16::DS),
            _ => Err(()),
        }
    }
}

#[derive(Serialize)]
pub struct CPU {
    pub ax: u16,
    pub bx: u16,
    pub cx: u16,
    pub dx: u16,
    pub si: u16,
    pub di: u16,
    pub bp: u16,
    pub sp: u16,
    pub ip: u16,
    pub cs: u16,
    pub ds: u16,
    pub es: u16,
    pub ss: u16,
    pub flags: u16,
    pub interrupts: VecDeque<u8>,
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
            flags: 0,
            interrupts: VecDeque::<u8>::new(),
        }
    }

    fn mutate_register<F: Fn(&mut u16, RegisterHalf) -> ()>(&mut self, register: &RegisterEncoding, mutation: F) {
        match register {
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX) => mutation(&mut self.ax, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::BX) => mutation(&mut self.bx, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::CX) => mutation(&mut self.cx, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX) => mutation(&mut self.dx, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::SI) => mutation(&mut self.si, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DI) => mutation(&mut self.di, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::BP) => mutation(&mut self.bp, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::SP) => mutation(&mut self.sp, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::ES) => mutation(&mut self.es, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::CS) => mutation(&mut self.cs, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::SS) => mutation(&mut self.ss, RegisterHalf::FULL),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DS) => mutation(&mut self.ds, RegisterHalf::FULL),
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

    pub fn read_register(&self, register: &RegisterEncoding) -> Value {
        match register {
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX) => Value::Word(self.ax),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::BX) => Value::Word(self.bx),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::CX) => Value::Word(self.cx),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX) => Value::Word(self.dx),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::SI) => Value::Word(self.si),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DI) => Value::Word(self.di),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::BP) => Value::Word(self.bp),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::SP) => Value::Word(self.sp),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::ES) => Value::Word(self.es),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::CS) => Value::Word(self.cs),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::SS) => Value::Word(self.ss),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DS) => Value::Word(self.ds),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH) => Value::Byte(self.ax.to_le_bytes()[1]),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL) => Value::Byte(self.ax.to_le_bytes()[0]),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::BH) => Value::Byte(self.bx.to_le_bytes()[1]),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::BL) => Value::Byte(self.bx.to_le_bytes()[0]),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CH) => Value::Byte(self.cx.to_le_bytes()[1]),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CL) => Value::Byte(self.cx.to_le_bytes()[0]),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::DH) => Value::Byte(self.dx.to_le_bytes()[1]),
            RegisterEncoding::RegisterEncoding8(RegisterEncoding8::DL) => Value::Byte(self.dx.to_le_bytes()[0]),
        }
    }

    pub fn read_register16(&self, register: &RegisterEncoding16) -> u16 {
        match register {
            RegisterEncoding16::AX => self.ax,
            RegisterEncoding16::BX => self.bx,
            RegisterEncoding16::CX => self.cx,
            RegisterEncoding16::DX => self.dx,
            RegisterEncoding16::SI => self.si,
            RegisterEncoding16::DI => self.di,
            RegisterEncoding16::BP => self.bp,
            RegisterEncoding16::SP => self.sp,
            RegisterEncoding16::ES => self.es,
            RegisterEncoding16::CS => self.cs,
            RegisterEncoding16::SS => self.ss,
            RegisterEncoding16::DS => self.ds,
        }
    }

    pub fn read_register8(&self, register: &RegisterEncoding8) -> u8 {
        match register {
            RegisterEncoding8::AH => self.ax.to_le_bytes()[1],
            RegisterEncoding8::AL => self.ax.to_le_bytes()[0],
            RegisterEncoding8::BH => self.bx.to_le_bytes()[1],
            RegisterEncoding8::BL => self.bx.to_le_bytes()[0],
            RegisterEncoding8::CH => self.cx.to_le_bytes()[1],
            RegisterEncoding8::CL => self.cx.to_le_bytes()[0],
            RegisterEncoding8::DH => self.dx.to_le_bytes()[1],
            RegisterEncoding8::DL => self.dx.to_le_bytes()[0],
        }
    }

    pub fn write_register(&mut self, register: &RegisterEncoding, value: &Value) {
        self.mutate_register(&register, |r: &mut u16, h: RegisterHalf| {
            match h {
                RegisterHalf::FULL => *r = match value {
                    Value::Word(w) => *w,
                    _ => unreachable!(),
                },
                RegisterHalf::HIGH => *r = match value {
                    Value::Byte(b) => {
                        let low = *r & 0x00FF;
                        low | ((*b as u16) << 8)
                    },
                    _ => unreachable!(),
                },
                RegisterHalf::LOW => *r = match value {
                    Value::Byte(b) => {
                        let high = *r & 0xFF00;
                        high | (*b as u16)
                    },
                    _ => unreachable!(),
                },
            }
        });
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Value {
    Byte(u8),
    Word(u16),
    Dword(u32),
}

impl TryFrom<u8> for Value {
    type Error = ();
    fn try_from(v: u8) -> Result<Self, Self::Error> {
        Ok(Value::Byte(v))
    }
}

impl TryFrom<u16> for Value {
    type Error = ();
    fn try_from(v: u16) -> Result<Self, Self::Error> {
        Ok(Value::Word(v))
    }
}

pub struct Emulator {
    pub cpu: CPU,
    pub ram: Box<[u8]>,
    pub disk: Box<[u8]>,
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Copy, Clone)]
#[repr(u8)]
enum Mod {
    NoDisplacement = 0b00,
    OneByteDisplacement = 0b01,
    TwoByteDisplacement = 0b10,
    Register = 0b11,
    Direct = 0xFF,
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
enum OperandDirection {
    ModRM = 0,
    Register = 1,
}

impl TryFrom<bool> for OperandDirection {
    type Error = ();
    fn try_from(v: bool) -> Result<Self, Self::Error> {
        match v {
            x if x == true => Ok(OperandDirection::Register),
            x if x == false => Ok(OperandDirection::ModRM),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
enum OperandSize {
    Byte = 0,
    Word = 1,
}

impl TryFrom<bool> for OperandSize {
    type Error = ();
    fn try_from(v: bool) -> Result<Self, Self::Error> {
        match v {
            x if x == true => Ok(OperandSize::Word),
            x if x == false => Ok(OperandSize::Byte),
            _ => Err(()),
        }
    }
}

fn sign_extend(value: u8) -> u16 {
    let sign = value.bit(7);
    let mut result = value as u16;
    if sign {
        result |= 0xFF00;
    }
    result
}

fn sign_extend_16(value: u16) -> u32 {
    let sign = value.bit(15);
    let mut result = value as u32;
    if sign {
        result |= 0xFFFF0000;
    }
    result
}

enum Operand {
    Register(RegisterEncoding),
    Memory(U20),
}

impl Emulator {
    pub fn new(disk: Box<[u8]>) -> Self {
        Emulator {
            cpu: CPU::new(),
            ram: vec![0; 1024 * 1024].into_boxed_slice(),
            disk,
        }
    }

    fn jump_to_interrupt_vector(&mut self, int: u8, insn_size: u16) {
        self.push_word(self.cpu.flags);
        self.cpu.flags &= !Flags::IF.bits();
        self.cpu.flags &= !Flags::TF.bits();
        self.push_word(self.cpu.cs);
        self.push_word(self.cpu.ip + insn_size);
        let vector_addr = int as u16 * 4;
        let isr_ip = self.read_word(&U20::new(0, vector_addr));
        let isr_cs = self.read_word(&U20::new(0, vector_addr + 2));
        self.cpu.cs = isr_cs;
        self.cpu.ip = isr_ip - insn_size;
    }

    fn iret(&mut self) {
        self.cpu.ip = self.pop_word();
        self.cpu.cs = self.pop_word();
        self.cpu.flags = self.pop_word();
    }

    pub fn service_interrupts(&mut self) {
        if (self.cpu.flags & Flags::IF.bits()) != 0 && !self.cpu.interrupts.is_empty() {
            let int = self.cpu.interrupts.pop_front().unwrap();
            self.jump_to_interrupt_vector(int, 0);
        }
    }

    fn run(&mut self, dont_init_ivt: bool) -> Result<(), Box<dyn std::error::Error>> {
        init_rom_configuration(self);
        // TODO: Fix tests and remove this conditional
        if !dont_init_ivt {
            init_ivt(self);
        }
        load_bootsector(self).unwrap();
        loop {
            self.service_interrupts();
            update_mda_screen(&self);
            match self.execute() {
                Ok(()) => (),
                Err(error) => return Err(error),
            }
        }
    }

    fn parse_modrm(operand_size: &OperandSize, modrm: &u8) -> (Mod, u8, RM) {
        let mut m: Mod = (modrm >> 6).try_into().unwrap();
        let opcode = (modrm & 0b00111000) >> 3;
        let rm = match m {
            Mod::Register => {
                match operand_size {
                    OperandSize::Byte => RM::Register(RegisterEncoding::RegisterEncoding8(RegisterEncoding8::try_from(modrm & 0b00000111).unwrap())),
                    OperandSize::Word => RM::Register(RegisterEncoding::RegisterEncoding16(RegisterEncoding16::try_from(modrm & 0b00000111).unwrap())),
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
                    _ => unreachable!(),
                }
            }
        };
        if modrm & 0b00000111 == 0b00000110 && m == Mod::NoDisplacement {
            m = Mod::Direct;
        }
        (m, opcode, rm)
    }

    fn get_operand_by_modrm(&self, address: &U20, modrmmod: &Mod, rm: &RM, segment_override: &Option<SegmentRegister>) -> Operand {
        match modrmmod {
            Mod::Register => {
                match rm {
                    RM::Register(e) => Operand::Register(*e),
                    _ => unreachable!(),
                }
            },
            _ => {
                Operand::Memory(self.calculate_address_by_modrm(&address, *modrmmod, *rm, &segment_override))
            },
        }
    }

    fn calculate_offset_by_rm(&self, rm: RM, displacement: u16) -> u16 {
        let mut offset: u16 = displacement;
        offset = offset + match rm {
            RM::BaseIndex(bi) => {
                bi.0.map_or(
                    0,
                    |r| {
                        match self.cpu.read_register(&RegisterEncoding::RegisterEncoding16(r)) {
                            Value::Word(w) => w,
                            _ => unreachable!(),
                        }
                    }
                ) + bi.1.map_or(
                    0,
                    |r| {
                        match self.cpu.read_register(&RegisterEncoding::RegisterEncoding16(r)) {
                            Value::Word(w) => w,
                            _ => unreachable!(),
                        }
                    }
                )
            }
            _ => unreachable!(),
        };
        offset
    }

    fn calculate_address_by_rm(&self, rm: RM, displacement: u16, segment_override: &Option<SegmentRegister>) -> U20 {
        let offset: u16 = self.calculate_offset_by_rm(rm, displacement);
        let use_ss = match rm {
            RM::BaseIndex(bi) => {
                match bi.0 {
                    Some(r) => {
                        if r == RegisterEncoding16::BP {
                            true
                        } else {
                            false
                        }
                    },
                    _ => false,
                }
            },
            _ => unreachable!(),
        };
        let segment = segment_override.as_ref().unwrap_or(if use_ss { &SegmentRegister::SS } else { &SegmentRegister::DS });
        let segment = match segment {
            SegmentRegister::ES => self.cpu.es,
            SegmentRegister::CS => self.cpu.cs,
            SegmentRegister::DS => self.cpu.ds,
            SegmentRegister::SS => self.cpu.ss,
        };
        U20::new(segment, offset)
    }

    fn get_instruction_size_extension_by_mod(modrmmod: &Mod) -> u16 {
        match modrmmod {
            Mod::Register => 0,
            Mod::NoDisplacement => 0,
            Mod::OneByteDisplacement => 1,
            Mod::TwoByteDisplacement => 2,
            Mod::Direct => 2,
        }
    }

    fn calculate_address_by_modrm(&self, instruction_address: &U20, modrmmod: Mod, rm: RM, segment_override: &Option<SegmentRegister>) -> U20 {
        match modrmmod {
            Mod::OneByteDisplacement => {
                let displacement = self.ram[instruction_address.0 as usize + 2] as u16;
                self.calculate_address_by_rm(rm, displacement, segment_override)
            },
            Mod::TwoByteDisplacement => {
                let displacement = self.ram[
                    instruction_address.0 as usize + 2
                ] as u16 | (self.ram[
                    instruction_address.0 as usize + 3]
                as u16) << 8;
                self.calculate_address_by_rm(rm, displacement, segment_override)
            },
            Mod::NoDisplacement => {
                self.calculate_address_by_rm(rm, 0, segment_override)
            },
            Mod::Direct => {
                let offset = self.ram[
                    instruction_address.0 as usize + 2
                ] as u16 | (self.ram[
                    instruction_address.0 as usize + 3
                ] as u16) << 8;
                let segment = segment_override.as_ref().unwrap_or(&SegmentRegister::DS);
                let segment = match segment {
                    SegmentRegister::ES => self.cpu.es,
                    SegmentRegister::CS => self.cpu.cs,
                    SegmentRegister::DS => self.cpu.ds,
                    SegmentRegister::SS => self.cpu.ss,
                };
                U20::new(segment, offset)
            },
            _ => unreachable!(),
        }
    }

    fn calculate_offset_by_modrm(&self, instruction_address: &U20, modrmmod: Mod, rm: RM) -> u16 {
        match modrmmod {
            Mod::OneByteDisplacement => {
                let displacement = self.ram[instruction_address.0 as usize + 2] as u16;
                self.calculate_offset_by_rm(rm, displacement)
            },
            Mod::TwoByteDisplacement => {
                let displacement = self.ram[
                    instruction_address.0 as usize + 2
                ] as u16 | (self.ram[
                    instruction_address.0 as usize + 3]
                as u16) << 8;
                self.calculate_offset_by_rm(rm, displacement)
            },
            Mod::NoDisplacement => {
                self.calculate_offset_by_rm(rm, 0)
            },
            Mod::Direct => {
                self.ram[
                    instruction_address.0 as usize + 2
                ] as u16 | (self.ram[
                    instruction_address.0 as usize + 3
                ] as u16) << 8
            },
            _ => unreachable!(),
        }
    }

    // TODO: Dispatch table?
    // TODO: Exceptions
    pub fn execute(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        if self.cpu.cs == BIOS_SEG {
            execute_isr(self, self.cpu.ip as u8);
        }
        let mut address = U20::new(self.cpu.cs, self.cpu.ip);
        // HLT
        if self.ram[address.0 as usize] == 0b11110100 {
            return Err("Halted".into());
        }
        let mut instruction_size = 1;
        let mut segment_override: Option<SegmentRegister> = None;
        let mut rep_while_zero: Option<bool> = None;
        // REP
        if self.ram[address.0 as usize] >> 1 == 0b1111001 {
            instruction_size += 1;
            let while_equal_zero = self.ram[address.0 as usize].bit(0);
            rep_while_zero = Some(while_equal_zero);
            address = U20::new(self.cpu.cs, self.cpu.ip + 1);
        }
        // Segment override prefix
        if self.ram[address.0 as usize] >> 5 == 0b00000001 && self.ram[address.0 as usize] & 0b00000111 == 0b00000110 {
            instruction_size += 1;
            segment_override = Some(((self.ram[address.0 as usize] & 0b00011000) >> 3).try_into().unwrap());
            address = U20::new(self.cpu.cs, self.cpu.ip + 1);
        }
        // LOOP
        if self.ram[address.0 as usize] == 0b11100010 {
            instruction_size += 1;
            let mut count = match self.cpu.read_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::CX)) {
                Value::Word(w) => w,
                _ => unreachable!(),
            };
            count -= 1;
            self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::CX), &Value::Word(count));
            if count != 0 {
                let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JCXZ
        if self.ram[address.0 as usize] == 0b11100011 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.cx == 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JMP, direct intrasegment
        if self.ram[address.0 as usize] == 0b11101001 {
            // instruction_size += 2;
            let diff = (self.ram[address.0 as usize + 1] as u16
                | ((self.ram[address.0 as usize + 2] as u16)) << 8)
                as i16 + 2;
            self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
            return Ok(())
        }
        // JMP, direct intrasegment short
        if self.ram[address.0 as usize] == 0b11101011 {
            // instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
            return Ok(())
        }
        // JMP, direct intersegment
        if self.ram[address.0 as usize] == 0b11101010 {
            // instruction_size += 4;
            let offset = (self.ram[address.0 as usize + 1] as u16
                | ((self.ram[address.0 as usize + 2] as u16)) << 8)
                as u16;
            let segment = (self.ram[address.0 as usize + 3] as u16
                | ((self.ram[address.0 as usize + 4] as u16)) << 8)
                as u16;
            self.cpu.ip = offset;
            self.cpu.cs = segment;
            return Ok(())
        }
        // CALL, direct intrasegment
        if self.ram[address.0 as usize] == 0b11101000 {
            instruction_size += 2;
            let diff = (self.ram[address.0 as usize + 1] as u16
                | ((self.ram[address.0 as usize + 2] as u16)) << 8)
                as i16 + 3;
            self.push_word(self.cpu.ip + instruction_size);
            self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
            return Ok(())
        }
        // CALL, direct intersegment
        if self.ram[address.0 as usize] == 0b10011010 {
            instruction_size += 4;
            let offset = (self.ram[address.0 as usize + 1] as u16
                | ((self.ram[address.0 as usize + 2] as u16)) << 8)
                as u16;
            let segment = (self.ram[address.0 as usize + 3] as u16
                | ((self.ram[address.0 as usize + 4] as u16)) << 8)
                as u16;
            self.push_word(self.cpu.cs);
            self.push_word(self.cpu.ip + instruction_size);
            self.cpu.ip = offset;
            self.cpu.cs = segment;
            return Ok(())
        }
        // RET, intrasegment
        if self.ram[address.0 as usize] == 0b11000011 {
            let offset = self.pop_word();
            self.cpu.ip = offset;
            return Ok(());
        }
        // RET, intrasegment, adding immediate to SP
        if self.ram[address.0 as usize] == 0b11000010 {
            // instruction_size += 2;
            let immediate = (self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16;
            self.cpu.ss += immediate;
            let offset = self.pop_word();
            self.cpu.ip = offset;
            return Ok(());
        }
        // RET, intersegment
        if self.ram[address.0 as usize] == 0b11001011 {
            let offset = self.pop_word();
            let segment = self.pop_word();
            self.cpu.ip = offset;
            self.cpu.cs = segment;
            return Ok(());
        }
        // RET, intersegment, adding immediate to SP
        if self.ram[address.0 as usize] == 0b11001010 {
            // instruction_size += 2;
            let immediate = (self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16;
            self.cpu.ss += immediate;
            let offset = self.pop_word();
            let segment = self.pop_word();
            self.cpu.ip = offset;
            self.cpu.cs = segment;
            return Ok(());
        }
        // JE/JZ
        if self.ram[address.0 as usize] == 0b01110100 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::ZF.bits() == 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JNE/JNZ
        if self.ram[address.0 as usize] == 0b01110101 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::ZF.bits() != 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JL/JNGE
        if self.ram[address.0 as usize] == 0b01111100 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if (self.cpu.flags & Flags::SF.bits()).count_ones() != (self.cpu.flags & Flags::OF.bits()).count_ones() {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JNL/JGE
        if self.ram[address.0 as usize] == 0b01111101 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if (self.cpu.flags & Flags::SF.bits()).count_ones() == (self.cpu.flags & Flags::OF.bits()).count_ones() {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JLE/JNG
        if self.ram[address.0 as usize] == 0b01111110 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if (self.cpu.flags & Flags::SF.bits()).count_ones() != (self.cpu.flags & Flags::OF.bits()).count_ones()
                && self.cpu.flags & Flags::ZF.bits() != 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JNLE/JG
        if self.ram[address.0 as usize] == 0b01111111 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if (self.cpu.flags & Flags::SF.bits()).count_ones() == (self.cpu.flags & Flags::OF.bits()).count_ones()
                && self.cpu.flags & Flags::ZF.bits() == 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JB/JNAE
        if self.ram[address.0 as usize] == 0b01110010 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::CF.bits() != 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JNB/JAE
        if self.ram[address.0 as usize] == 0b01110011 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::CF.bits() == 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JBE/JNA
        if self.ram[address.0 as usize] == 0b01110110 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::CF.bits() != 0 || self.cpu.flags & Flags::ZF.bits() != 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JNBE/JA
        if self.ram[address.0 as usize] == 0b01110111 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::CF.bits() == 0 && self.cpu.flags & Flags::ZF.bits() == 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JP/JPE
        if self.ram[address.0 as usize] == 0b01111010 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::PF.bits() != 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JNP/JPO
        if self.ram[address.0 as usize] == 0b01111011 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::PF.bits() == 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JO
        if self.ram[address.0 as usize] == 0b01110000 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::OF.bits() != 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JNO
        if self.ram[address.0 as usize] == 0b01110001 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::OF.bits() == 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JS
        if self.ram[address.0 as usize] == 0b01111000 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::SF.bits() != 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // JNS
        if self.ram[address.0 as usize] == 0b01111001 {
            instruction_size += 1;
            let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
            if self.cpu.flags & Flags::SF.bits() == 0 {
                self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                return Ok(())
            }
        }
        // IRET
        if self.ram[address.0 as usize] == 0b11001111 {
            instruction_size -= 1;
            self.iret();
        }
        // INT, specified type
        if self.ram[address.0 as usize] == 0b11001101 {
            instruction_size += 1;
            let immediate = self.ram[address.0 as usize + 1];
            self.jump_to_interrupt_vector(immediate, 2);
        }
        // INT, type 3
        if self.ram[address.0 as usize] == 0b11001100 {
            self.jump_to_interrupt_vector(3, 1);
        }
        // INTO
        if self.ram[address.0 as usize] == 0b11001110 {
            if self.cpu.flags & Flags::OF.bits() != 0 {
                self.jump_to_interrupt_vector(4, 1);
            }
        }
        // CLC
        if self.ram[address.0 as usize] == 0b11111000 {
            self.cpu.flags &= !Flags::CF.bits();
        }
        // CMC
        if self.ram[address.0 as usize] == 0b11110101 {
            let carry = (self.cpu.flags & Flags::CF.bits()).count_ones();
            if carry != 0 {
                self.cpu.flags &= !Flags::CF.bits();
            } else {
                self.cpu.flags |= Flags::CF.bits();
            }
        }
        // STC
        if self.ram[address.0 as usize] == 0b11111001 {
            self.cpu.flags |= Flags::CF.bits();
        }
        // CLD
        if self.ram[address.0 as usize] == 0b11111100 {
            self.cpu.flags &= !Flags::DF.bits();
        }
        // STD
        if self.ram[address.0 as usize] == 0b11111101 {
            self.cpu.flags |= Flags::DF.bits();
        }
        // CLI
        if self.ram[address.0 as usize] == 0b11111010 {
            self.cpu.flags &= !Flags::IF.bits();
        }
        // STI
        if self.ram[address.0 as usize] == 0b11111011 {
            self.cpu.flags |= Flags::IF.bits();
        }
        // WAIT
        if self.ram[address.0 as usize] == 0b10011011 {
            // TODO
            // No-op?
        }
        // LOCK
        if self.ram[address.0 as usize] == 0b11110000 {
            // TODO
        }
        // PUSH, segment register
        if self.ram[address.0 as usize] & 0b11100111 == 0b00000110 {
            let segment: SegmentRegister = ((self.ram[address.0 as usize] & 0b00011000) >> 3).try_into().unwrap();
            let segment: RegisterEncoding16 = segment.try_into().unwrap();
            let value = self.cpu.read_register(&RegisterEncoding::RegisterEncoding16(segment));
            match value {
                Value::Word(w) => self.push_word(w),
                _ => unreachable!(),
            };
        }
        // POP, segment register
        if self.ram[address.0 as usize] & 0b11100111 == 0b00000111 {
            let segment: SegmentRegister = ((self.ram[address.0 as usize] & 0b00011000) >> 3).try_into().unwrap();
            if segment == SegmentRegister::CS {
                self.cpu.ip += instruction_size;
                return Ok(());
            }
            let segment: RegisterEncoding16 = segment.try_into().unwrap();
            self.pop_word_into_operand(&Operand::Register(RegisterEncoding::RegisterEncoding16(segment)));
        }
        match self.ram[address.0 as usize] >> 1 {
            // LOOPNZ/LOOPZ
            0b1110000 => {
                instruction_size += 1;
                let not_z = self.ram[address.0 as usize].bit(0);
                let mut count = match self.cpu.read_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::CX)) {
                    Value::Word(w) => w,
                    _ => unreachable!(),
                };
                let valid = (not_z && (self.cpu.flags & Flags::ZF.bits()).count_ones() == 0) || (!not_z && (self.cpu.flags & Flags::ZF.bits()).count_ones() != 0);
                count -= 1;
                self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::CX), &Value::Word(count));
                if count != 0 && valid {
                    let diff = i16::from(self.ram[address.0 as usize + 1] as i8) + 2;
                    self.cpu.ip = self.cpu.ip.wrapping_add_signed(diff);
                    return Ok(())
                }
            },
            // TEST, modr/m
            0b1000010 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                let register_value = self.cpu.read_register(&register);
                let result = match operand_value {
                    Value::Word(w) => match register_value {
                        Value::Word(w2) => Value::Word(w & w2),
                        _ => unreachable!(),
                    },
                    Value::Byte(b) => match register_value {
                        Value::Byte(b2) => Value::Byte(b & b2),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                self.cpu.flags &= !Flags::CF.bits();
                self.cpu.flags &= !Flags::OF.bits();
                self.update_flags("SZP", None, Some(result), None);
            },
            // MOVS
            0b1010010 => {
                loop {
                    let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                    let source_segment = match segment_override {
                        Some(ref s) => s,
                        None => &SegmentRegister::DS,
                    };
                    let source_segment = match source_segment {
                        SegmentRegister::ES => self.cpu.es,
                        SegmentRegister::CS => self.cpu.cs,
                        SegmentRegister::DS => self.cpu.ds,
                        SegmentRegister::SS => self.cpu.ss,
                    };
                    let source = U20::new(source_segment, self.cpu.read_register16(&RegisterEncoding16::SI));
                    let destination = U20::new(self.cpu.es, self.cpu.read_register16(&RegisterEncoding16::DI));
                    match operand_size {
                        OperandSize::Byte => self.ram[destination.0 as usize] = self.ram[source.0 as usize],
                        OperandSize::Word => {
                            let value = self.read_word(&source);
                            self.write_word(&destination, value);
                        },
                    };
                    if self.cpu.flags & Flags::DF.bits() == 0 {
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.si += 1;
                                self.cpu.di += 1;
                            },
                            OperandSize::Word => {
                                self.cpu.si += 2;
                                self.cpu.di += 2;
                            },
                        };
                    } else {
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.si -= 1;
                                self.cpu.di -= 1;
                            },
                            OperandSize::Word => {
                                self.cpu.si -= 2;
                                self.cpu.di -= 2;
                            },
                        };
                    }
                    match rep_while_zero {
                        None => break,
                        _ => {
                            self.cpu.cx -= 1;
                            if self.cpu.cx == 0 {
                                break;
                            }
                        },
                    };
                }
            },
            // CMPS
            0b1010011 => {
                loop {
                    let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                    let source_segment = match segment_override {
                        Some(ref s) => s,
                        None => &SegmentRegister::DS,
                    };
                    let source_segment = match source_segment {
                        SegmentRegister::ES => self.cpu.es,
                        SegmentRegister::CS => self.cpu.cs,
                        SegmentRegister::DS => self.cpu.ds,
                        SegmentRegister::SS => self.cpu.ss,
                    };
                    let source1 = U20::new(source_segment, self.cpu.read_register16(&RegisterEncoding16::SI));
                    let source2 = U20::new(self.cpu.es, self.cpu.read_register16(&RegisterEncoding16::DI));
                    let source1_value = match operand_size {
                        OperandSize::Byte => Value::Byte(self.ram[source1.0 as usize]),
                        OperandSize::Word => Value::Word(self.read_word(&source1)),
                    };
                    let source2_value = match operand_size {
                        OperandSize::Byte => Value::Byte(self.ram[source2.0 as usize]),
                        OperandSize::Word => Value::Word(self.read_word(&source2)),
                    };
                    let sum: Value = match source1_value {
                        Value::Byte(b) => Value::Byte(b - match source2_value {
                            Value::Byte(b2) => b2,
                            _ => unreachable!(),
                        }),
                        Value::Word(w) => Value::Word(w - match source2_value {
                            Value::Word(w2) => w2,
                            _ => unreachable!(),
                        }),
                        _ => unreachable!(),
                    };
                    self.update_flags("CZSOPA", Some(source1_value), Some(sum), Some(false));
                    if self.cpu.flags & Flags::DF.bits() == 0 {
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.si += 1;
                                self.cpu.di += 1;
                            },
                            OperandSize::Word => {
                                self.cpu.si += 2;
                                self.cpu.di += 2;
                            },
                        };
                    } else {
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.si -= 1;
                                self.cpu.di -= 1;
                            },
                            OperandSize::Word => {
                                self.cpu.si -= 2;
                                self.cpu.di -= 2;
                            },
                        };
                    }
                    match rep_while_zero {
                        None => break,
                        Some(b) => {
                            self.cpu.cx -= 1;
                            if self.cpu.cx == 0 {
                                break;
                            }
                            if b && self.cpu.flags & Flags::ZF.bits() == 0 {
                                break;
                            }
                            if !b && self.cpu.flags & Flags::ZF.bits() != 0 {
                                break;
                            }
                        },
                    };
                }
            },
            // SCAS
            0b1010111 => {
                loop {
                    let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                    let source = U20::new(self.cpu.es, self.cpu.read_register16(&RegisterEncoding16::DI));
                    let source_value = match operand_size {
                        OperandSize::Byte => Value::Byte(self.ram[source.0 as usize]),
                        OperandSize::Word => Value::Word(self.read_word(&source)),
                    };
                    let register_value = match operand_size {
                        OperandSize::Byte => self.cpu.read_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL)),
                        OperandSize::Word => self.cpu.read_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX)),
                    };
                    let sum: Value = match register_value {
                        Value::Byte(b) => Value::Byte(b - match source_value {
                            Value::Byte(b2) => b2,
                            _ => unreachable!(),
                        }),
                        Value::Word(w) => Value::Word(w - match source_value {
                            Value::Word(w2) => w2,
                            _ => unreachable!(),
                        }),
                        _ => unreachable!(),
                    };
                    self.update_flags("CZSOPA", Some(register_value), Some(sum), Some(false));
                    if self.cpu.flags & Flags::DF.bits() == 0 {
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.si += 1;
                                self.cpu.di += 1;
                            },
                            OperandSize::Word => {
                                self.cpu.si += 2;
                                self.cpu.di += 2;
                            },
                        };
                    } else {
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.si -= 1;
                                self.cpu.di -= 1;
                            },
                            OperandSize::Word => {
                                self.cpu.si -= 2;
                                self.cpu.di -= 2;
                            },
                        };
                    }
                    match rep_while_zero {
                        None => break,
                        Some(b) => {
                            self.cpu.cx -= 1;
                            if self.cpu.cx == 0 {
                                break;
                            }
                            if b && self.cpu.flags & Flags::ZF.bits() == 0 {
                                break;
                            }
                            if !b && self.cpu.flags & Flags::ZF.bits() != 0 {
                                break;
                            }
                        },
                    };
                }
            },
            // LODS
            0b1010110 => {
                loop {
                    let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                    let source_segment = match segment_override {
                        Some(ref s) => s,
                        None => &SegmentRegister::DS,
                    };
                    let source_segment = match source_segment {
                        SegmentRegister::ES => self.cpu.es,
                        SegmentRegister::CS => self.cpu.cs,
                        SegmentRegister::DS => self.cpu.ds,
                        SegmentRegister::SS => self.cpu.ss,
                    };
                    let source = U20::new(source_segment, self.cpu.read_register16(&RegisterEncoding16::SI));
                    let source_value = match operand_size {
                        OperandSize::Byte => Value::Byte(self.ram[source.0 as usize]),
                        OperandSize::Word => Value::Word(self.read_word(&source)),
                    };
                    match source_value {
                        Value::Byte(b) => self.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL), &Value::Byte(b)),
                        Value::Word(w) => self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX), &Value::Word(w)),
                        _ => unreachable!(),
                    };
                    if self.cpu.flags & Flags::DF.bits() == 0 {
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.si += 1;
                                self.cpu.di += 1;
                            },
                            OperandSize::Word => {
                                self.cpu.si += 2;
                                self.cpu.di += 2;
                            },
                        };
                    } else {
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.si -= 1;
                                self.cpu.di -= 1;
                            },
                            OperandSize::Word => {
                                self.cpu.si -= 2;
                                self.cpu.di -= 2;
                            },
                        };
                    }
                    match rep_while_zero {
                        None => break,
                        _ => {
                            self.cpu.cx -= 1;
                            if self.cpu.cx == 0 {
                                break;
                            }
                        },
                    };
                }
            },
            // STOS
            0b1010101 => {
                loop {
                    let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                    let destination = U20::new(self.cpu.es, self.cpu.read_register16(&RegisterEncoding16::DI));
                    match operand_size {
                        OperandSize::Byte => self.ram[destination.0 as usize] = self.cpu.read_register8(&RegisterEncoding8::AL),
                        OperandSize::Word => self.write_word(&destination, self.cpu.read_register16(&RegisterEncoding16::AX)),
                    };
                    if self.cpu.flags & Flags::DF.bits() == 0 {
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.si += 1;
                                self.cpu.di += 1;
                            },
                            OperandSize::Word => {
                                self.cpu.si += 2;
                                self.cpu.di += 2;
                            },
                        };
                    } else {
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.si -= 1;
                                self.cpu.di -= 1;
                            },
                            OperandSize::Word => {
                                self.cpu.si -= 2;
                                self.cpu.di -= 2;
                            },
                        };
                    }
                    match rep_while_zero {
                        None => break,
                        _ => {
                            self.cpu.cx -= 1;
                            if self.cpu.cx == 0 {
                                break;
                            }
                        },
                    };
                }
            },
            0b1111111 => {
                instruction_size += 1;
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                if self.ram[address.0 as usize].bit(0) {
                    match modrm.1 {
                        // JMP, indirect intrasegment
                        0b100 => {
                            let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                            let operand_value = self.get_operand_value(&operand, &OperandSize::Word);
                            self.cpu.ip = match operand_value {
                                Value::Word(w) => w,
                                _ => unreachable!(),
                            };
                            return Ok(());
                        },
                        // JMP, indirect intersegment
                        0b101 => {
                            let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                            match operand {
                                Operand::Memory(m) => {
                                     let offset = (self.ram[m.0 as usize + 0] as u16
                                         | ((self.ram[m.0 as usize + 1] as u16)) << 8)
                                         as u16;
                                     let segment = (self.ram[m.0 as usize + 2] as u16
                                         | ((self.ram[m.0 as usize + 3] as u16)) << 8)
                                         as u16;
                                     self.cpu.ip = offset;
                                     self.cpu.cs = segment;
                                     return Ok(());
                                },
                                _ => (),
                            };
                        },
                        // CALL, indirect intrasegment
                        0b010 => {
                            let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                            let operand_value = self.get_operand_value(&operand, &OperandSize::Word);
                            self.push_word(self.cpu.ip + instruction_size);
                            self.cpu.ip = match operand_value {
                                Value::Word(w) => w,
                                _ => unreachable!(),
                            };
                            return Ok(());
                        },
                        // CALL, indirect intersegment
                        0b011 => {
                            let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                            match operand {
                                Operand::Memory(m) => {
                                     let offset = (self.ram[m.0 as usize + 0] as u16
                                         | ((self.ram[m.0 as usize + 1] as u16)) << 8)
                                         as u16;
                                     let segment = (self.ram[m.0 as usize + 2] as u16
                                         | ((self.ram[m.0 as usize + 3] as u16)) << 8)
                                         as u16;
                                     self.push_word(self.cpu.cs);
                                     self.push_word(self.cpu.ip + instruction_size);
                                     self.cpu.ip = offset;
                                     self.cpu.cs = segment;
                                     return Ok(());
                                },
                                _ => (),
                            };
                        },
                        _ => (),
                    }
                }
                match modrm.1 {
                    // INC, modr/m
                    0b000 => {
                        let (before, after) = match modrm.0 {
                            Mod::Register => {
                                match modrm.2 {
                                    RM::Register(e) => {
                                        let before = self.cpu.read_register(&e);
                                        self.inc_register(&e);
                                        let after = self.cpu.read_register(&e);
                                        (before, after)
                                    }
                                    _ => unreachable!(),
                                }
                            },
                            _ => {
                                let address = self.calculate_address_by_modrm(&address, modrm.0, modrm.2, &segment_override);
                                let before = self.read_word(&address);
                                self.write_word(&address, before + 1);
                                let after = self.read_word(&address);
                                (Value::Word(before), Value::Word(after))
                            },
                        };
                        self.update_flags("ZSOPA", Some(before), Some(after), Some(true))
                    },
                    // DEC, modr/m
                    0b001 => {
                        let (before, after) = match modrm.0 {
                            Mod::Register => {
                                match modrm.2 {
                                    RM::Register(e) => {
                                        let before = self.cpu.read_register(&e);
                                        self.dec_register(&e);
                                        let after = self.cpu.read_register(&e);
                                        (before, after)
                                    }
                                    _ => unreachable!(),
                                }
                            },
                            _ => {
                                let address = self.calculate_address_by_modrm(&address, modrm.0, modrm.2, &segment_override);
                                let before = self.read_word(&address);
                                self.write_word(&address, before - 1);
                                let after = self.read_word(&address);
                                (Value::Word(before), Value::Word(after))
                            },
                        };
                        self.update_flags("ZSOPA", Some(before), Some(after), Some(false))
                    },
                    // PUSH, modr/m
                    0b00000110 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &OperandSize::Word);
                        match operand_value {
                            Value::Word(w) => self.push_word(w),
                            _ => unreachable!(),
                        };
                    },
                    _ => unreachable!(),
                };
            },
            0b1100011 => {
                instruction_size += 1;
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                match modrm.1 {
                    // MOV, mod/rm, immediate
                    0b00000000 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + 2])
                            },
                            OperandSize::Word => {
                                instruction_size += 2;
                                Value::Word((self.ram[address.0 as usize + 3] as u16) << 8 | self.ram[address.0 as usize + 2] as u16)
                            },
                        };
                        self.write_operand(&operand, &immediate);
                    },
                    _ => unreachable!(),
                };
            },
            // MOV, mem to AX/AL
            0b1010000 => {
                instruction_size += 2;
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                };
                let address = U20::new(self.cpu.ds, (self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16);
                let value = match operand_size {
                    OperandSize::Byte => Value::Byte(self.ram[address.0 as usize]),
                    OperandSize::Word => Value::Word(self.read_word(&address)),
                };
                self.cpu.write_register(&register, &value);
            },
            // MOV, AX/AL to mem
            0b1010001 => {
                instruction_size += 2;
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                };
                let address = U20::new(self.cpu.ds, (self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16);
                let value = self.cpu.read_register(&register);
                match value {
                    Value::Byte(b) => self.ram[address.0 as usize] = b,
                    Value::Word(w) => self.write_word(&address, w),
                    _ => unreachable!(),
                };
            },
            // XCHG, modr/m
            0b1000011 => {
                instruction_size += 1;
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let register_value = self.cpu.read_register(&register);
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                self.cpu.write_register(&register, &operand_value);
                self.write_operand(&operand, &register_value);
            },
            // IN, direct
            0b1110010 => {
                instruction_size += 1;
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let port = self.ram[address.0 as usize + 1];
                let value = self.read_port(port as u16, &operand_size);
                let register = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                };
                self.cpu.write_register(&register, &value);
            },
            // IN, indirect
            0b1110110 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let port = self.cpu.read_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX));
                let value = match port {
                    Value::Word(w) => self.read_port(w, &operand_size),
                    _ => unreachable!(),
                };
                let register = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                };
                self.cpu.write_register(&register, &value);
            },
            // OUT, direct
            0b1110011 => {
                instruction_size += 1;
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let port = self.ram[address.0 as usize + 1];
                let register = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                };
                let value = self.cpu.read_register(&register);
                self.write_port(port as u16, value);
            },
            // OUT, indirect
            0b1110111 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let port = self.cpu.read_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX));
                let register = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                };
                let value = self.cpu.read_register(&register);
                match port {
                    Value::Word(w) => self.write_port(w, value),
                    _ => unreachable!(),
                };
            },
            // ADD, immediate with AX/AL
            0b0000010 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => {
                        RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL)
                    },
                    OperandSize::Word => {
                        RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX)
                    },
                };
                let register_value = self.cpu.read_register(&register);
                let immediate = match operand_size {
                    OperandSize::Byte => {
                        instruction_size += 1;
                        Value::Byte(self.ram[address.0 as usize + 1])
                    },
                    OperandSize::Word => {
                        instruction_size += 2;
                        Value::Word((self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16)
                    },
                };
                let sum: Value = match register_value {
                    Value::Byte(b) => (b + match immediate {
                        Value::Byte(b2) => b2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    Value::Word(w) => (w + match immediate {
                        Value::Word(w2) => w2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    _ => unreachable!(),
                };
                self.cpu.write_register(&register, &sum);
                self.update_flags("CZSOPA", Some(register_value), Some(sum), Some(true));
            },
            // ADC, immediate with AX/AL
            0b0001010 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => {
                        RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL)
                    },
                    OperandSize::Word => {
                        RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX)
                    },
                };
                let register_value = self.cpu.read_register(&register);
                let immediate = match operand_size {
                    OperandSize::Byte => {
                        instruction_size += 1;
                        Value::Byte(self.ram[address.0 as usize + 1])
                    },
                    OperandSize::Word => {
                        instruction_size += 2;
                        Value::Word((self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16)
                    },
                };
                let cf = (self.cpu.flags & Flags::CF.bits()).count_ones();
                let sum: Value = match register_value {
                    Value::Byte(b) => (b + cf as u8 + match immediate {
                        Value::Byte(b2) => b2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    Value::Word(w) => (w + cf as u16 + match immediate {
                        Value::Word(w2) => w2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    _ => unreachable!(),
                };
                self.cpu.write_register(&register, &sum);
                self.update_flags("CZSOPA", Some(register_value), Some(sum), Some(true));
            },
            // SUB, immediate with AX/AL
            0b0010110 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => {
                        RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL)
                    },
                    OperandSize::Word => {
                        RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX)
                    },
                };
                let register_value = self.cpu.read_register(&register);
                let immediate = match operand_size {
                    OperandSize::Byte => {
                        instruction_size += 1;
                        Value::Byte(self.ram[address.0 as usize + 1])
                    },
                    OperandSize::Word => {
                        instruction_size += 2;
                        Value::Word((self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16)
                    },
                };
                let sum: Value = match register_value {
                    Value::Byte(b) => (b - match immediate {
                        Value::Byte(b2) => b2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    Value::Word(w) => (w - match immediate {
                        Value::Word(w2) => w2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    _ => unreachable!(),
                };
                self.cpu.write_register(&register, &sum);
                self.update_flags("CZSOPA", Some(register_value), Some(sum), Some(false));
            },
            // CMP, immediate with AX/AL
            0b0011110 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => {
                        RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL)
                    },
                    OperandSize::Word => {
                        RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX)
                    },
                };
                let register_value = self.cpu.read_register(&register);
                let immediate = match operand_size {
                    OperandSize::Byte => {
                        instruction_size += 1;
                        Value::Byte(self.ram[address.0 as usize + 1])
                    },
                    OperandSize::Word => {
                        instruction_size += 2;
                        Value::Word((self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16)
                    },
                };
                let sum: Value = match register_value {
                    Value::Byte(b) => (b - match immediate {
                        Value::Byte(b2) => b2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    Value::Word(w) => (w - match immediate {
                        Value::Word(w2) => w2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    _ => unreachable!(),
                };
                self.update_flags("CZSOPA", Some(register_value), Some(sum), Some(false));
            },
            // SBB, immediate with AX/AL
            0b0001110 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => {
                        RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL)
                    },
                    OperandSize::Word => {
                        RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX)
                    },
                };
                let register_value = self.cpu.read_register(&register);
                let immediate = match operand_size {
                    OperandSize::Byte => {
                        instruction_size += 1;
                        Value::Byte(self.ram[address.0 as usize + 1])
                    },
                    OperandSize::Word => {
                        instruction_size += 2;
                        Value::Word((self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16)
                    },
                };
                let cf = (self.cpu.flags & Flags::CF.bits()).count_ones();
                let sum: Value = match register_value {
                    Value::Byte(b) => (b - cf as u8 - match immediate {
                        Value::Byte(b2) => b2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    Value::Word(w) => (w - cf as u16 - match immediate {
                        Value::Word(w2) => w2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    _ => unreachable!(),
                };
                self.cpu.write_register(&register, &sum);
                self.update_flags("CZSOPA", Some(register_value), Some(sum), Some(false));
            },
            0b1111011 => {
                instruction_size += 1;
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                match modrm.1 {
                    // NEG, modr/m
                    0b011 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let neg = match operand_value {
                            Value::Byte(b) => Value::Byte((b as i8).neg() as u8),
                            Value::Word(w) => Value::Word((w as i16).neg() as u16),
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &neg);
                        let zero = match operand_size {
                            OperandSize::Byte => Value::Byte(0u8),
                            OperandSize::Word => Value::Word(0u16),
                        };
                        self.update_flags("CZSOPA", Some(zero), Some(neg), Some(false));
                    },
                    // NOT, modr/m
                    0b010 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let not = match operand_value {
                            Value::Byte(b) => Value::Byte(!b),
                            Value::Word(w) => Value::Word(!w),
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &not);
                    },
                    // MUL, modr/m
                    0b100 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let register = match operand_size {
                            OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                            OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                        };
                        let register_value = self.cpu.read_register(&register);
                        let result = match operand_value {
                            Value::Byte(b) => b as u32 * match register_value {
                                Value::Byte(b2) => b2 as u32,
                                _ => unreachable!(),
                            },
                            Value::Word(w) => w as u32 * match register_value {
                                Value::Word(w2) => w2 as u32,
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        };
                        match operand_size {
                            OperandSize::Byte => {
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX), &Value::Word(result as u16));
                                let high_order_bits = result as u16 >> 8;
                                if high_order_bits > 0 {
                                    self.cpu.flags |= Flags::CF.bits();
                                    self.cpu.flags |= Flags::OF.bits();
                                } else {
                                    self.cpu.flags &= !Flags::CF.bits();
                                    self.cpu.flags &= !Flags::OF.bits();
                                }
                            },
                            OperandSize::Word => {
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX), &Value::Word(result as u16));
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX), &Value::Word((result >> 16) as u16));
                                let high_order_bits = (result >> 16) as u16;
                                if high_order_bits > 0 {
                                    self.cpu.flags |= Flags::CF.bits();
                                    self.cpu.flags |= Flags::OF.bits();
                                } else {
                                    self.cpu.flags &= !Flags::CF.bits();
                                    self.cpu.flags &= !Flags::OF.bits();
                                }
                            },
                        };
                    },
                    // IMUL, modr/m
                    0b101 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let register = match operand_size {
                            OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                            OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                        };
                        let register_value = self.cpu.read_register(&register);
                        let result = match operand_value {
                            Value::Byte(b) => Value::Word((b as i16 * match register_value {
                                Value::Byte(b2) => b2 as i16,
                                _ => unreachable!(),
                            }) as u16),
                            Value::Word(w) => Value::Dword((sign_extend_16(w) as i32 * match register_value {
                                Value::Word(w2) => sign_extend_16(w2) as i32,
                                _ => unreachable!(),
                            }) as u32),
                            _ => unreachable!(),
                        };
                        match operand_size {
                            OperandSize::Byte => {
                                match result {
                                    Value::Word(w) => {
                                        self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX), &Value::Word(w));
                                        if sign_extend(w.bits(0..8) as u8) == w {
                                            self.cpu.flags |= Flags::CF.bits();
                                            self.cpu.flags |= Flags::OF.bits();
                                        } else {
                                            self.cpu.flags &= !Flags::CF.bits();
                                            self.cpu.flags &= !Flags::OF.bits();
                                        }
                                    },
                                    _ => unreachable!(),
                                };
                            },
                            OperandSize::Word => {
                                match result {
                                    Value::Dword(dw) => {
                                        self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX), &Value::Word(dw as u16));
                                        self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX), &Value::Word((dw >> 16) as u16));
                                        if sign_extend_16(dw.bits(0..16) as u16) == dw {
                                            self.cpu.flags |= Flags::CF.bits();
                                            self.cpu.flags |= Flags::OF.bits();
                                        } else {
                                            self.cpu.flags &= !Flags::CF.bits();
                                            self.cpu.flags &= !Flags::OF.bits();
                                        }
                                    },
                                    _ => unreachable!(),
                                };
                            },
                        };
                    },
                    // DIV, modr/m
                    0b110 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let register = RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX);
                        let register_value: u16 = match self.cpu.read_register(&register) {
                            Value::Word(w) => w,
                            _ => unreachable!(),
                        };
                        let register2 = RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX);
                        let register2_value: u16 = match self.cpu.read_register(&register2) {
                            Value::Word(w) => w,
                            _ => unreachable!(),
                        };
                        match operand_value {
                            Value::Byte(b) => {
                                if b == 0 {
                                    // TODO: DE
                                }
                            },
                            Value::Word(w) => {
                                if w == 0 {
                                    // TODO: DE
                                }
                            },
                            _ => unreachable!(),
                        };
                        match operand_value {
                            Value::Byte(b) => {
                                let result = register_value / b as u16;
                                let remainder = register_value % b as u16;
                                if result > 0xFF {
                                    // TODO: DE
                                }
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL), &Value::Byte(result as u8));
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH), &Value::Byte(remainder as u8));
                            },
                            Value::Word(w) => {
                                let dividend = (register2_value as u32) << 16 | register_value as u32;
                                let result = dividend / w as u32;
                                let remainder = dividend % w as u32;
                                if result > 0xFFFF {
                                    // TODO: DE
                                }
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX), &Value::Word(result as u16));
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX), &Value::Word(remainder as u16));
                            },
                            _ => unreachable!(),
                        };
                    },
                    // IDIV, modr/m
                    0b111 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let register = RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX);
                        let register_value: u16 = match self.cpu.read_register(&register) {
                            Value::Word(w) => w,
                            _ => unreachable!(),
                        };
                        let register2 = RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX);
                        let register2_value: u16 = match self.cpu.read_register(&register2) {
                            Value::Word(w) => w,
                            _ => unreachable!(),
                        };
                        match operand_value {
                            Value::Byte(b) => {
                                if b == 0 {
                                    // TODO: DE
                                }
                            },
                            Value::Word(w) => {
                                if w == 0 {
                                    // TODO: DE
                                }
                            },
                            _ => unreachable!(),
                        };
                        match operand_value {
                            Value::Byte(b) => {
                                let result = register_value as i16 / b as i16;
                                let remainder = register_value as i16 % b as i16;
                                if result > 0x7F || result < 0x80 {
                                    // TODO: DE
                                }
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL), &Value::Byte(result as u8));
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH), &Value::Byte(remainder as u8));
                            },
                            Value::Word(w) => {
                                let dividend = (register2_value as i32) << 16 | register_value as i32;
                                let result = dividend / w as i32;
                                let remainder = dividend % w as i32;
                                if result > 0x7FFF || result < 0x8000 {
                                    // TODO: DE
                                }
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX), &Value::Word(result as u16));
                                self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX), &Value::Word(remainder as u16));
                            },
                            _ => unreachable!(),
                        };
                    },
                    // TEST, modr/m, immediate
                    0b000 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let displacement_length = Self::get_instruction_size_extension_by_mod(&modrm.0) as usize;
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + displacement_length + 1])
                            },
                            OperandSize::Word => {
                                instruction_size += 2;
                                Value::Word((self.ram[address.0 as usize + displacement_length + 2] as u16) << 8 | self.ram[address.0 as usize + displacement_length + 1] as u16)
                            },
                        };
                        let result = match operand_value {
                            Value::Word(w) => match immediate {
                                Value::Word(w2) => Value::Word(w & w2),
                                _ => unreachable!(),
                            },
                            Value::Byte(b) => match immediate {
                                Value::Byte(b2) => Value::Byte(b & b2),
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        };
                        self.cpu.flags &= !Flags::CF.bits();
                        self.cpu.flags &= !Flags::OF.bits();
                        self.update_flags("SZP", None, Some(result), None);
                    },
                    _ => unreachable!(),
                };
            },
            0b1000000 => {
                instruction_size += 1;
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                match modrm.1 {
                    // AND, modr/m, immediate
                    0b100 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let displacement_length = Self::get_instruction_size_extension_by_mod(&modrm.0) as usize;
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + displacement_length + 1])
                            },
                            OperandSize::Word => {
                                instruction_size += 2;
                                Value::Word((self.ram[address.0 as usize + displacement_length + 2] as u16) << 8 | self.ram[address.0 as usize + displacement_length + 1] as u16)
                            },
                        };
                        let result = match operand_value {
                            Value::Word(w) => match immediate {
                                Value::Word(w2) => Value::Word(w & w2),
                                _ => unreachable!(),
                            },
                            Value::Byte(b) => match immediate {
                                Value::Byte(b2) => Value::Byte(b & b2),
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &result);
                        self.cpu.flags &= !Flags::CF.bits();
                        self.cpu.flags &= !Flags::OF.bits();
                        self.update_flags("SZP", None, Some(result), None);
                    },
                    // OR, modr/m, immediate
                    0b001 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let displacement_length = Self::get_instruction_size_extension_by_mod(&modrm.0) as usize;
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + displacement_length + 1])
                            },
                            OperandSize::Word => {
                                instruction_size += 2;
                                Value::Word((self.ram[address.0 as usize + displacement_length + 2] as u16) << 8 | self.ram[address.0 as usize + displacement_length + 1] as u16)
                            },
                        };
                        let result = match operand_value {
                            Value::Word(w) => match immediate {
                                Value::Word(w2) => Value::Word(w | w2),
                                _ => unreachable!(),
                            },
                            Value::Byte(b) => match immediate {
                                Value::Byte(b2) => Value::Byte(b | b2),
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &result);
                        self.cpu.flags &= !Flags::CF.bits();
                        self.cpu.flags &= !Flags::OF.bits();
                        self.update_flags("SZP", None, Some(result), None);
                    },
                    // XOR, modr/m, immediate
                    0b110 => {
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let displacement_length = Self::get_instruction_size_extension_by_mod(&modrm.0) as usize;
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + displacement_length + 1])
                            },
                            OperandSize::Word => {
                                instruction_size += 2;
                                Value::Word((self.ram[address.0 as usize + displacement_length + 2] as u16) << 8 | self.ram[address.0 as usize + displacement_length + 1] as u16)
                            },
                        };
                        let result = match operand_value {
                            Value::Word(w) => match immediate {
                                Value::Word(w2) => Value::Word(w ^ w2),
                                _ => unreachable!(),
                            },
                            Value::Byte(b) => match immediate {
                                Value::Byte(b2) => Value::Byte(b ^ b2),
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &result);
                        self.cpu.flags &= !Flags::CF.bits();
                        self.cpu.flags &= !Flags::OF.bits();
                        self.update_flags("SZP", None, Some(result), None);
                    },
                    _ => (),
                }
            },
            // AND, immediate with AX/AL
            0b0010010 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                };
                let immediate = match operand_size {
                    OperandSize::Byte => {
                        instruction_size += 1;
                        Value::Byte(self.ram[address.0 as usize + 1])
                    },
                    OperandSize::Word => {
                        instruction_size += 2;
                        Value::Word((self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16)
                    },
                };
                let register_value = self.cpu.read_register(&register);
                let result = match immediate {
                    Value::Word(w) => match register_value {
                        Value::Word(w2) => Value::Word(w & w2),
                        _ => unreachable!(),
                    },
                    Value::Byte(b) => match register_value {
                        Value::Byte(b2) => Value::Byte(b & b2),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                self.cpu.write_register(&register, &result);
                self.cpu.flags &= !Flags::CF.bits();
                self.cpu.flags &= !Flags::OF.bits();
                self.update_flags("SZP", None, Some(result), None);
            },
            // OR, immediate with AX/AL
            0b0000110 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                };
                let immediate = match operand_size {
                    OperandSize::Byte => {
                        instruction_size += 1;
                        Value::Byte(self.ram[address.0 as usize + 1])
                    },
                    OperandSize::Word => {
                        instruction_size += 2;
                        Value::Word((self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16)
                    },
                };
                let register_value = self.cpu.read_register(&register);
                let result = match immediate {
                    Value::Word(w) => match register_value {
                        Value::Word(w2) => Value::Word(w | w2),
                        _ => unreachable!(),
                    },
                    Value::Byte(b) => match register_value {
                        Value::Byte(b2) => Value::Byte(b | b2),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                self.cpu.write_register(&register, &result);
                self.cpu.flags &= !Flags::CF.bits();
                self.cpu.flags &= !Flags::OF.bits();
                self.update_flags("SZP", None, Some(result), None);
            },
            // XOR, immediate with AX/AL
            0b0011010 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                };
                let immediate = match operand_size {
                    OperandSize::Byte => {
                        instruction_size += 1;
                        Value::Byte(self.ram[address.0 as usize + 1])
                    },
                    OperandSize::Word => {
                        instruction_size += 2;
                        Value::Word((self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16)
                    },
                };
                let register_value = self.cpu.read_register(&register);
                let result = match immediate {
                    Value::Word(w) => match register_value {
                        Value::Word(w2) => Value::Word(w ^ w2),
                        _ => unreachable!(),
                    },
                    Value::Byte(b) => match register_value {
                        Value::Byte(b2) => Value::Byte(b ^ b2),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                self.cpu.write_register(&register, &result);
                self.cpu.flags &= !Flags::CF.bits();
                self.cpu.flags &= !Flags::OF.bits();
                self.update_flags("SZP", None, Some(result), None);
            },
            // TEST, immediate with AX/AL
            0b1010100 => {
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let register = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX),
                };
                let immediate = match operand_size {
                    OperandSize::Byte => {
                        instruction_size += 1;
                        Value::Byte(self.ram[address.0 as usize + 1])
                    },
                    OperandSize::Word => {
                        instruction_size += 2;
                        Value::Word((self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16)
                    },
                };
                let register_value = self.cpu.read_register(&register);
                let result = match immediate {
                    Value::Word(w) => match register_value {
                        Value::Word(w2) => Value::Word(w & w2),
                        _ => unreachable!(),
                    },
                    Value::Byte(b) => match register_value {
                        Value::Byte(b2) => Value::Byte(b & b2),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                self.cpu.flags &= !Flags::CF.bits();
                self.cpu.flags &= !Flags::OF.bits();
                self.update_flags("SZP", None, Some(result), None);
            },
            _ => (),
        }
        match self.ram[address.0 as usize] >> 2 {
            // ADD, modr/m
            0b000000 => {
                instruction_size += 1;
                let direction: OperandDirection = ((self.ram[address.0 as usize] & 0b00000010) >> 1).try_into().unwrap();
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let register_value = self.cpu.read_register(&register);
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                let sum: Value = match register_value {
                    Value::Byte(b) => (b + match operand_value {
                        Value::Byte(b2) => b2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    Value::Word(w) => (w + match operand_value {
                        Value::Word(w2) => w2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    _ => unreachable!(),
                };
                match direction {
                    OperandDirection::Register => {
                        self.cpu.write_register(&register, &sum);
                    },
                    OperandDirection::ModRM => {
                        self.write_operand(&operand, &sum);
                    },
                }
                self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(true));
            },
            // ADC, modr/m
            0b000100 => {
                instruction_size += 1;
                let direction: OperandDirection = ((self.ram[address.0 as usize] & 0b00000010) >> 1).try_into().unwrap();
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let register_value = self.cpu.read_register(&register);
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                let cf = (self.cpu.flags & Flags::CF.bits()).count_ones();
                let sum: Value = match register_value {
                    Value::Byte(b) => (b + cf as u8 + match operand_value {
                        Value::Byte(b2) => b2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    Value::Word(w) => (w + cf as u16 + match operand_value {
                        Value::Word(w2) => w2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    _ => unreachable!(),
                };
                match direction {
                    OperandDirection::Register => {
                        self.cpu.write_register(&register, &sum);
                    },
                    OperandDirection::ModRM => {
                        self.write_operand(&operand, &sum);
                    },
                }
                self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(true));
            },
            // SUB, modr/m
            0b001010 => {
                instruction_size += 1;
                let direction: OperandDirection = ((self.ram[address.0 as usize] & 0b00000010) >> 1).try_into().unwrap();
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let register_value = self.cpu.read_register(&register);
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                let sum: Value = match direction {
                    OperandDirection::ModRM => match operand_value {
                        Value::Byte(b) => (b - match register_value {
                            Value::Byte(b2) => b2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        Value::Word(w) => (w - match register_value {
                            Value::Word(w2) => w2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        _ => unreachable!(),
                    },
                    OperandDirection::Register => match register_value {
                        Value::Byte(b) => (b - match operand_value {
                            Value::Byte(b2) => b2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        Value::Word(w) => (w - match operand_value {
                            Value::Word(w2) => w2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        _ => unreachable!(),
                    },
                };
                match direction {
                    OperandDirection::Register => {
                        self.cpu.write_register(&register, &sum);
                    },
                    OperandDirection::ModRM => {
                        self.write_operand(&operand, &sum);
                    },
                }
                self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(false));
            },
            // CMP, modr/m
            0b001110 => {
                instruction_size += 1;
                let direction: OperandDirection = ((self.ram[address.0 as usize] & 0b00000010) >> 1).try_into().unwrap();
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let register_value = self.cpu.read_register(&register);
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                let sum: Value = match direction {
                    OperandDirection::ModRM => match operand_value {
                        Value::Byte(b) => (b - match register_value {
                            Value::Byte(b2) => b2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        Value::Word(w) => (w - match register_value {
                            Value::Word(w2) => w2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        _ => unreachable!(),
                    },
                    OperandDirection::Register => match register_value {
                        Value::Byte(b) => (b - match operand_value {
                            Value::Byte(b2) => b2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        Value::Word(w) => (w - match operand_value {
                            Value::Word(w2) => w2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        _ => unreachable!(),
                    },
                };
                self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(false));
            },
            // SBB, modr/m
            0b000110 => {
                instruction_size += 1;
                let direction: OperandDirection = ((self.ram[address.0 as usize] & 0b00000010) >> 1).try_into().unwrap();
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let register_value = self.cpu.read_register(&register);
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                let cf = (self.cpu.flags & Flags::CF.bits()).count_ones();
                let sum: Value = match direction {
                    OperandDirection::ModRM => match operand_value {
                        Value::Byte(b) => (b - cf as u8 - match register_value {
                            Value::Byte(b2) => b2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        Value::Word(w) => (w - match register_value {
                            Value::Word(w2) => w2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        _ => unreachable!(),
                    },
                    OperandDirection::Register => match register_value {
                        Value::Byte(b) => (b - cf as u8 - match operand_value {
                            Value::Byte(b2) => b2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        Value::Word(w) => (w - match operand_value {
                            Value::Word(w2) => w2,
                            _ => unreachable!(),
                        }).try_into().unwrap(),
                        _ => unreachable!(),
                    },
                };
                match direction {
                    OperandDirection::Register => {
                        self.cpu.write_register(&register, &sum);
                    },
                    OperandDirection::ModRM => {
                        self.write_operand(&operand, &sum);
                    },
                }
                self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(false));
            },
            // MOV, mod/rm
            0b00100010 => {
                instruction_size += 1;
                let direction: OperandDirection = self.ram[address.0 as usize].bit(1).try_into().unwrap();
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                let register_value = self.cpu.read_register(&register);
                match direction {
                    OperandDirection::Register => {
                        self.cpu.write_register(&register, &operand_value);
                    },
                    OperandDirection::ModRM => {
                        match register_value {
                            Value::Byte(b) => {
                                match operand {
                                    Operand::Memory(m) => self.ram[m.0 as usize] = b,
                                    _ => unreachable!(),
                                };
                            },
                            Value::Word(w) => {
                                match operand {
                                    Operand::Memory(m) => self.write_word(&m, w),
                                    _ => unreachable!(),
                                };
                            },
                            _ => unreachable!(),
                        };
                    },
                };
            },
            0b100000 => {
                let mut sign_extend = self.ram[address.0 as usize].bit(1);
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                match operand_size {
                    OperandSize::Byte => sign_extend = false,
                    _ => (),
                };
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                match modrm.1 {
                    // ADD, modr/m, immediate
                    0b000 => {
                        instruction_size += 1;
                        instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let displacement_length = Self::get_instruction_size_extension_by_mod(&modrm.0) as usize;
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + displacement_length + 1])
                            },
                            OperandSize::Word => {
                                if !sign_extend {
                                    instruction_size += 2;
                                    Value::Word((self.ram[address.0 as usize + displacement_length + 3] as u16) << 8 | self.ram[address.0 as usize + displacement_length + 2] as u16)
                                } else {
                                    instruction_size += 1;
                                    Value::Word(crate::emulator::sign_extend(self.ram[address.0 as usize + displacement_length + 2]))
                                }
                            },
                        };
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let sum: Value = match immediate {
                            Value::Byte(b) => (b + match operand_value {
                                Value::Byte(b2) => b2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            Value::Word(w) => (w + match operand_value {
                                Value::Word(w2) => w2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &sum);
                        self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(true));
                    },
                    // ADC, modr/m, immediate
                    0b010 => {
                        instruction_size += 1;
                        instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let displacement_length = Self::get_instruction_size_extension_by_mod(&modrm.0) as usize;
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + displacement_length + 1])
                            },
                            OperandSize::Word => {
                                if !sign_extend {
                                    instruction_size += 2;
                                    Value::Word((self.ram[address.0 as usize + displacement_length + 3] as u16) << 8 | self.ram[address.0 as usize + displacement_length + 2] as u16)
                                } else {
                                    instruction_size += 1;
                                    Value::Word(crate::emulator::sign_extend(self.ram[address.0 as usize + displacement_length + 2]))
                                }
                            },
                        };
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let cf = (self.cpu.flags & Flags::CF.bits()).count_ones();
                        let sum: Value = match immediate {
                            Value::Byte(b) => (b + cf as u8 + match operand_value {
                                Value::Byte(b2) => b2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            Value::Word(w) => (w + cf as u16 + match operand_value {
                                Value::Word(w2) => w2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &sum);
                        self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(true));
                    },
                    // SUB, modr/m, immediate
                    0b101 => {
                        instruction_size += 1;
                        instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let displacement_length = Self::get_instruction_size_extension_by_mod(&modrm.0) as usize;
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + displacement_length + 1])
                            },
                            OperandSize::Word => {
                                if !sign_extend {
                                    instruction_size += 2;
                                    Value::Word((self.ram[address.0 as usize + displacement_length + 3] as u16) << 8 | self.ram[address.0 as usize + displacement_length + 2] as u16)
                                } else {
                                    instruction_size += 1;
                                    Value::Word(crate::emulator::sign_extend(self.ram[address.0 as usize + displacement_length + 2]))
                                }
                            },
                        };
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let sum = match operand_value {
                            Value::Byte(b) => (b - match immediate {
                                Value::Byte(b2) => b2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            Value::Word(w) => (w - match immediate {
                                Value::Word(w2) => w2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &sum);
                        self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(false));
                    },
                    // CMP, modr/m, immediate
                    0b111 => {
                        instruction_size += 1;
                        instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let displacement_length = Self::get_instruction_size_extension_by_mod(&modrm.0) as usize;
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + displacement_length + 1])
                            },
                            OperandSize::Word => {
                                if !sign_extend {
                                    instruction_size += 2;
                                    Value::Word((self.ram[address.0 as usize + displacement_length + 3] as u16) << 8 | self.ram[address.0 as usize + displacement_length + 2] as u16)
                                } else {
                                    instruction_size += 1;
                                    Value::Word(crate::emulator::sign_extend(self.ram[address.0 as usize + displacement_length + 2]))
                                }
                            },
                        };
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let sum = match operand_value {
                            Value::Byte(b) => (b - match immediate {
                                Value::Byte(b2) => b2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            Value::Word(w) => (w - match immediate {
                                Value::Word(w2) => w2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            _ => unreachable!(),
                        };
                        self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(false));
                    },
                    // SBB, modr/m, immediate
                    0b011 => {
                        instruction_size += 1;
                        instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let displacement_length = Self::get_instruction_size_extension_by_mod(&modrm.0) as usize;
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + displacement_length + 1])
                            },
                            OperandSize::Word => {
                                if !sign_extend {
                                    instruction_size += 2;
                                    Value::Word((self.ram[address.0 as usize + displacement_length + 3] as u16) << 8 | self.ram[address.0 as usize + displacement_length + 2] as u16)
                                } else {
                                    instruction_size += 1;
                                    Value::Word(crate::emulator::sign_extend(self.ram[address.0 as usize + 2]))
                                }
                            },
                        };
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let cf = (self.cpu.flags & Flags::CF.bits()).count_ones();
                        let sum: Value = match operand_value {
                            Value::Byte(b) => (b - cf as u8 - match immediate {
                                Value::Byte(b2) => b2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            Value::Word(w) => (w - cf as u16 - match immediate {
                                Value::Word(w2) => w2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &sum);
                        self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(false));
                    },
                    _ => (),
                }
            },
            0b100011 => {
                // MOV, modr/m, segment register
                if !self.ram[address.0 as usize].bit(0) {
                    instruction_size += 1;
                    let direction: OperandDirection = self.ram[address.0 as usize].bit(1).try_into().unwrap();
                    let modrm = Self::parse_modrm(&OperandSize::Word, &self.ram[address.0 as usize + 1]);
                    instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                    let register: SegmentRegister = modrm.1.try_into().unwrap();
                    if direction == OperandDirection::Register && register == SegmentRegister::CS {
                        self.cpu.ip += instruction_size;
                        return Ok(());
                    }
                    let register: RegisterEncoding16 = register.try_into().unwrap();
                    let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                    match direction {
                        OperandDirection::Register => {
                            let operand_value = self.get_operand_value(&operand, &OperandSize::Word);
                            self.cpu.mutate_register(&RegisterEncoding::RegisterEncoding16(register), |r: &mut u16, _h: RegisterHalf| {
                                match operand_value {
                                    Value::Word(w) => *r = w,
                                    _ => unreachable!(),
                                };
                            });
                        },
                        OperandDirection::ModRM => {
                            let register_value = self.cpu.read_register(&RegisterEncoding::RegisterEncoding16(register));
                            self.write_operand(&operand, &register_value);
                        },
                    };
                }
            },
            // AND, modr/m
            0b001000 => {
                let direction: OperandDirection = self.ram[address.0 as usize].bit(1).try_into().unwrap();
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                let register_value = self.cpu.read_register(&register);
                let result = match operand_value {
                    Value::Word(w) => match register_value {
                        Value::Word(w2) => Value::Word(w & w2),
                        _ => unreachable!(),
                    },
                    Value::Byte(b) => match register_value {
                        Value::Byte(b2) => Value::Byte(b & b2),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                match direction {
                    OperandDirection::Register => {
                        self.cpu.write_register(&register, &result);
                    },
                    OperandDirection::ModRM => {
                        match result {
                            Value::Byte(b) => {
                                match operand {
                                    Operand::Memory(m) => self.ram[m.0 as usize] = b,
                                    _ => unreachable!(),
                                };
                            },
                            Value::Word(w) => {
                                match operand {
                                    Operand::Memory(m) => self.write_word(&m, w),
                                    _ => unreachable!(),
                                };
                            },
                            _ => unreachable!(),
                        };
                    },
                };
                self.cpu.flags &= !Flags::CF.bits();
                self.cpu.flags &= !Flags::OF.bits();
                self.update_flags("SZP", None, Some(result), None);
            },
            // OR, modr/m
            0b000010 => {
                instruction_size += 1;
                let direction: OperandDirection = self.ram[address.0 as usize].bit(1).try_into().unwrap();
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                let register_value = self.cpu.read_register(&register);
                let result = match operand_value {
                    Value::Word(w) => match register_value {
                        Value::Word(w2) => Value::Word(w | w2),
                        _ => unreachable!(),
                    },
                    Value::Byte(b) => match register_value {
                        Value::Byte(b2) => Value::Byte(b | b2),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                match direction {
                    OperandDirection::Register => {
                        self.cpu.write_register(&register, &result);
                    },
                    OperandDirection::ModRM => {
                        match result {
                            Value::Byte(b) => {
                                match operand {
                                    Operand::Memory(m) => self.ram[m.0 as usize] = b,
                                    _ => unreachable!(),
                                };
                            },
                            Value::Word(w) => {
                                match operand {
                                    Operand::Memory(m) => self.write_word(&m, w),
                                    _ => unreachable!(),
                                };
                            },
                            _ => unreachable!(),
                        };
                    },
                };
                self.cpu.flags &= !Flags::CF.bits();
                self.cpu.flags &= !Flags::OF.bits();
                self.update_flags("SZP", None, Some(result), None);
            },
            // XOR, modr/m
            0b001100 => {
                instruction_size += 1;
                let direction: OperandDirection = self.ram[address.0 as usize].bit(1).try_into().unwrap();
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                let operand_value = self.get_operand_value(&operand, &operand_size);
                let register_value = self.cpu.read_register(&register);
                let result = match operand_value {
                    Value::Word(w) => match register_value {
                        Value::Word(w2) => Value::Word(w ^ w2),
                        _ => unreachable!(),
                    },
                    Value::Byte(b) => match register_value {
                        Value::Byte(b2) => Value::Byte(b ^ b2),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                match direction {
                    OperandDirection::Register => {
                        self.cpu.write_register(&register, &result);
                    },
                    OperandDirection::ModRM => {
                        match result {
                            Value::Byte(b) => {
                                match operand {
                                    Operand::Memory(m) => self.ram[m.0 as usize] = b,
                                    _ => unreachable!(),
                                };
                            },
                            Value::Word(w) => {
                                match operand {
                                    Operand::Memory(m) => self.write_word(&m, w),
                                    _ => unreachable!(),
                                };
                            },
                            _ => unreachable!(),
                        };
                    },
                };
                self.cpu.flags &= !Flags::CF.bits();
                self.cpu.flags &= !Flags::OF.bits();
                self.update_flags("SZP", None, Some(result), None);
            },
            0b110100 => {
                instruction_size += 1;
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                match modrm.1 {
                    // SHL/SAL, mod/rm
                    0b100 => {
                        let mut variable_shift = self.ram[address.0 as usize].bit(1);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let count = match variable_shift {
                            true => self.cpu.read_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CL)),
                            false => Value::Byte(1),
                        };
                        if count == Value::Byte(1) {
                            variable_shift = false;
                        }
                        if !variable_shift {
                            match operand_value {
                                Value::Word(w) => {
                                    if w.bit(15) == w.bit(14) {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    } else {
                                        self.cpu.flags |= Flags::OF.bits();
                                    }
                                },
                                Value::Byte(b) => {
                                    if b.bit(7) == b.bit(6) {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    } else {
                                        self.cpu.flags |= Flags::OF.bits();
                                    }
                                },
                                _ => unreachable!(),
                            };
                        }
                        let mut result = operand_value;
                        match count {
                            Value::Byte(b) => {
                                for _ in 0..b {
                                    let before = result.clone();
                                    result = match result {
                                        Value::Word(w) => {
                                            Value::Word(w << 1)
                                        },
                                        Value::Byte(b) => {
                                            Value::Byte(b << 1)
                                        },
                                        _ => unreachable!(),
                                    };
                                    self.update_carry_flag(&before, &result);
                                }
                            },
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &result);
                    },
                    // SHR, modr/m
                    0b101 => {
                        let mut variable_shift = self.ram[address.0 as usize].bit(1);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let count = match variable_shift {
                            true => self.cpu.read_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CL)),
                            false => Value::Byte(1),
                        };
                        if count == Value::Byte(1) {
                            variable_shift = false;
                        }
                        if !variable_shift {
                            match operand_value {
                                Value::Word(w) => {
                                    if w.bit(15) {
                                        self.cpu.flags |= Flags::OF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    }
                                },
                                Value::Byte(b) => {
                                    if b.bit(7) {
                                        self.cpu.flags |= Flags::OF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    }
                                },
                                _ => unreachable!(),
                            };
                        }
                        let mut result = operand_value;
                        match count {
                            Value::Byte(b) => {
                                for _ in 0..b {
                                    let before = result.clone();
                                    result = match result {
                                        Value::Word(w) => {
                                            Value::Word(w >> 1)
                                        },
                                        Value::Byte(b) => {
                                            Value::Byte(b >> 1)
                                        },
                                        _ => unreachable!(),
                                    };
                                    match before {
                                        Value::Word(w) => {
                                            if w.bit(0) {
                                                self.cpu.flags |= Flags::CF.bits();
                                            } else {
                                                self.cpu.flags &= !Flags::CF.bits();
                                            }
                                        },
                                        Value::Byte(b) => {
                                            if b.bit(0) {
                                                self.cpu.flags |= Flags::CF.bits();
                                            } else {
                                                self.cpu.flags &= !Flags::CF.bits();
                                            }
                                        },
                                        _ => unreachable!(),
                                    };
                                }
                            },
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &result);
                    },
                    // SAR, modr/m
                    0b111 => {
                        let mut variable_shift = self.ram[address.0 as usize].bit(1);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let count = match variable_shift {
                            true => self.cpu.read_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CL)),
                            false => Value::Byte(1),
                        };
                        if count == Value::Byte(1) {
                            variable_shift = false;
                        }
                        if !variable_shift {
                            self.cpu.flags &= !Flags::OF.bits();
                        }
                        let mut result = operand_value;
                        match count {
                            Value::Byte(b) => {
                                for _ in 0..b {
                                    let before = result.clone();
                                    result = match result {
                                        Value::Word(w) => {
                                            Value::Word(((w as i16) >> 1) as u16)
                                        },
                                        Value::Byte(b) => {
                                            Value::Byte(((b as i8) >> 1) as u8)
                                        },
                                        _ => unreachable!(),
                                    };
                                    match before {
                                        Value::Word(w) => {
                                            if w.bit(0) {
                                                self.cpu.flags |= Flags::CF.bits();
                                            } else {
                                                self.cpu.flags &= !Flags::CF.bits();
                                            }
                                        },
                                        Value::Byte(b) => {
                                            if b.bit(0) {
                                                self.cpu.flags |= Flags::CF.bits();
                                            } else {
                                                self.cpu.flags &= !Flags::CF.bits();
                                            }
                                        },
                                        _ => unreachable!(),
                                    };
                                }
                            },
                            _ => unreachable!(),
                        };
                        self.write_operand(&operand, &result);
                    },
                    // ROL, modr/m
                    0b000 => {
                        let mut variable_shift = self.ram[address.0 as usize].bit(1);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let count = match variable_shift {
                            true => self.cpu.read_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CL)),
                            false => Value::Byte(1),
                        };
                        if count == Value::Byte(1) {
                            variable_shift = false;
                        }
                        let mut result = operand_value;
                        match count {
                            Value::Byte(b) => {
                                for _ in 0..b {
                                    let before_msb;
                                    result = match result {
                                        Value::Word(w) => {
                                            before_msb = w.bit(15);
                                            Value::Word(w.rotate_left(1))
                                        },
                                        Value::Byte(b) => {
                                            before_msb = b.bit(7);
                                            Value::Byte(b.rotate_left(1))
                                        },
                                        _ => unreachable!(),
                                    };
                                    if before_msb {
                                        self.cpu.flags |= Flags::CF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::CF.bits();
                                    }
                                }
                            },
                            _ => unreachable!(),
                        };
                        if !variable_shift {
                            match result {
                                Value::Word(w) => {
                                    if (w.bit(15) as u32 ^ (self.cpu.flags & Flags::CF.bits()).count_ones()) != 0 {
                                        self.cpu.flags |= Flags::OF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    }
                                },
                                Value::Byte(b) => {
                                    if (b.bit(7) as u32 ^ (self.cpu.flags & Flags::CF.bits()).count_ones()) != 0 {
                                        self.cpu.flags |= Flags::OF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    }
                                },
                                _ => unreachable!(),
                            };
                        }
                        self.write_operand(&operand, &result);
                    },
                    // ROR, modr/m
                    0b001 => {
                        let mut variable_shift = self.ram[address.0 as usize].bit(1);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let count = match variable_shift {
                            true => self.cpu.read_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CL)),
                            false => Value::Byte(1),
                        };
                        if count == Value::Byte(1) {
                            variable_shift = false;
                        }
                        let mut result = operand_value;
                        match count {
                            Value::Byte(b) => {
                                for _ in 0..b {
                                    let before_lsb;
                                    result = match result {
                                        Value::Word(w) => {
                                            before_lsb = w.bit(0);
                                            Value::Word(w.rotate_right(1))
                                        },
                                        Value::Byte(b) => {
                                            before_lsb = b.bit(0);
                                            Value::Byte(b.rotate_right(1))
                                        },
                                        _ => unreachable!(),
                                    };
                                    if before_lsb {
                                        self.cpu.flags |= Flags::CF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::CF.bits();
                                    }
                                }
                            },
                            _ => unreachable!(),
                        };
                        if !variable_shift {
                            match result {
                                Value::Word(w) => {
                                    if (w.bit(15) as u32 ^ w.bit(14) as u32) != 0 {
                                        self.cpu.flags |= Flags::OF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    }
                                },
                                Value::Byte(b) => {
                                    if (b.bit(7) as u32 ^ b.bit(6) as u32) != 0 {
                                        self.cpu.flags |= Flags::OF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    }
                                },
                                _ => unreachable!(),
                            };
                        }
                        self.write_operand(&operand, &result);
                    },
                    // RCL, modr/m
                    0b010 => {
                        let mut variable_shift = self.ram[address.0 as usize].bit(1);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let count = match variable_shift {
                            true => self.cpu.read_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CL)),
                            false => Value::Byte(1),
                        };
                        if count == Value::Byte(1) {
                            variable_shift = false;
                        }
                        let mut result = operand_value;
                        match count {
                            Value::Byte(b) => {
                                for _ in 0..b {
                                    let before_msb;
                                    result = match result {
                                        Value::Word(w) => {
                                            before_msb = w.bit(15);
                                            Value::Word((w << 1) | ((self.cpu.flags & Flags::CF.bits()).count_ones() as u16))
                                        },
                                        Value::Byte(b) => {
                                            before_msb = b.bit(7);
                                            Value::Byte((b << 1) | ((self.cpu.flags & Flags::CF.bits()).count_ones() as u8))
                                        },
                                        _ => unreachable!(),
                                    };
                                    if before_msb {
                                        self.cpu.flags |= Flags::CF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::CF.bits();
                                    }
                                }
                            },
                            _ => unreachable!(),
                        };
                        if !variable_shift {
                            match result {
                                Value::Word(w) => {
                                    if (w.bit(15) as u32 ^ (self.cpu.flags & Flags::CF.bits()).count_ones()) != 0 {
                                        self.cpu.flags |= Flags::OF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    }
                                },
                                Value::Byte(b) => {
                                    if (b.bit(7) as u32 ^ (self.cpu.flags & Flags::CF.bits()).count_ones()) != 0 {
                                        self.cpu.flags |= Flags::OF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    }
                                },
                                _ => unreachable!(),
                            };
                        }
                        self.write_operand(&operand, &result);
                    },
                    // RCR, modr/m
                    0b011 => {
                        let mut variable_shift = self.ram[address.0 as usize].bit(1);
                        let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                        let operand_value = self.get_operand_value(&operand, &operand_size);
                        let count = match variable_shift {
                            true => self.cpu.read_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::CL)),
                            false => Value::Byte(1),
                        };
                        if count == Value::Byte(1) {
                            variable_shift = false;
                        }
                        let mut result = operand_value;
                        match count {
                            Value::Byte(b) => {
                                for _ in 0..b {
                                    let before_lsb;
                                    result = match result {
                                        Value::Word(w) => {
                                            before_lsb = w.bit(0);
                                            Value::Word((w >> 1) | (((self.cpu.flags & Flags::CF.bits()).count_ones() as u16) << 15))
                                        },
                                        Value::Byte(b) => {
                                            before_lsb = b.bit(0);
                                            Value::Byte((b >> 1) | (((self.cpu.flags & Flags::CF.bits()).count_ones() as u8) << 7))
                                        },
                                        _ => unreachable!(),
                                    };
                                    if before_lsb {
                                        self.cpu.flags |= Flags::CF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::CF.bits();
                                    }
                                }
                            },
                            _ => unreachable!(),
                        };
                        if !variable_shift {
                            match result {
                                Value::Word(w) => {
                                    if (w.bit(15) as u32 ^ w.bit(14) as u32) != 0 {
                                        self.cpu.flags |= Flags::OF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    }
                                },
                                Value::Byte(b) => {
                                    if (b.bit(7) as u32 ^ b.bit(6) as u32) != 0 {
                                        self.cpu.flags |= Flags::OF.bits();
                                    } else {
                                        self.cpu.flags &= !Flags::OF.bits();
                                    }
                                },
                                _ => unreachable!(),
                            };
                        }
                        self.write_operand(&operand, &result);
                    },
                    _ => unreachable!(),
                };
            },
            _ => (),
        }
        match self.ram[address.0 as usize] >> 3 {
            // INC, register-mode
            0b01000 => {
                let register = RegisterEncoding::RegisterEncoding16((self.ram[address.0 as usize] & 0b00000111).try_into().unwrap());
                let before = self.cpu.read_register(&register);
                self.inc_register(&register);
                let after = self.cpu.read_register(&register);
                self.update_flags("ZSOPA", Some(before), Some(after), Some(true))
            },
            // DEC, register-mode
            0b01001 => {
                let register = RegisterEncoding::RegisterEncoding16((self.ram[address.0 as usize] & 0b00000111).try_into().unwrap());
                let before = self.cpu.read_register(&register);
                self.dec_register(&register);
                let after = self.cpu.read_register(&register);
                self.update_flags("ZSOPA", Some(before), Some(after), Some(false))
            },
            // PUSH, register-mode
            0b01010 => {
                let register = RegisterEncoding::RegisterEncoding16((self.ram[address.0 as usize] & 0b00000111).try_into().unwrap());
                let value = self.cpu.read_register(&register);
                match value {
                    Value::Word(w) => self.push_word(w),
                    _ => unreachable!(),
                };
            },
            // POP, register-mode
            0b01011 => {
                let register = RegisterEncoding::RegisterEncoding16((self.ram[address.0 as usize] & 0b00000111).try_into().unwrap());
                self.pop_word_into_operand(&Operand::Register(register));
            },
            // XCHG, with AX, register-mode
            0b10010 => {
                let register = RegisterEncoding::RegisterEncoding16((self.ram[address.0 as usize] & 0b00000111).try_into().unwrap());
                let accumulator = RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX);
                let register_value = self.cpu.read_register(&register);
                let accumulator_value = self.cpu.read_register(&accumulator);
                self.cpu.mutate_register(&register, |r: &mut u16, _h: RegisterHalf| {
                    match accumulator_value {
                        Value::Word(w) => *r = w,
                        _ => unreachable!(),
                    };
                });
                self.cpu.mutate_register(&accumulator, |r: &mut u16, _h: RegisterHalf| {
                    match register_value {
                        Value::Word(w) => *r = w,
                        _ => unreachable!(),
                    };
                });
            },
            // ESC, modr/m
            0b11011 => {
                // TODO
                // No-op?
                instruction_size += 1;
            },
            _ => (),
        }
        match self.ram[address.0 as usize] >> 4 {
            // MOV, immediate, register-mode
            0b1011 => {
                let operand_size: OperandSize = ((self.ram[address.0 as usize] & 0b00001000) >> 3).try_into().unwrap();
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8((self.ram[address.0 as usize] & 0b00000111).try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16((self.ram[address.0 as usize] & 0b00000111).try_into().unwrap()),
                };
                let immediate = match operand_size {
                    OperandSize::Byte => {
                        instruction_size += 1;
                        Value::Byte(self.ram[address.0 as usize + 1])
                    },
                    OperandSize::Word => {
                        instruction_size += 2;
                        Value::Word((self.ram[address.0 as usize + 2] as u16) << 8 | self.ram[address.0 as usize + 1] as u16)
                    },
                };
                self.cpu.write_register(&register, &immediate);
            },
            _ => (),
        }
        if self.ram[address.0 as usize] == 0b10001111 {
            instruction_size += 1;
            let modrm = Self::parse_modrm(&OperandSize::Word, &self.ram[address.0 as usize + 1]);
            match modrm.1 {
                // POP, modr/m
                0b000 => {
                    instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                    let operand = self.get_operand_by_modrm(&address, &modrm.0, &modrm.2, &segment_override);
                    self.pop_word_into_operand(&operand);
                },
                _ => (),
            };
        }
        // XLAT
        if self.ram[address.0 as usize] == 0b11010111 {
            let table_offset = self.cpu.read_register16(&RegisterEncoding16::BX);
            let register_value = self.cpu.read_register8(&RegisterEncoding8::AL);
            let table_address = U20::new(self.cpu.ds, table_offset + register_value as u16);
            let value = Value::Byte(self.ram[table_address.0 as usize]);
            self.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL), &value);
        }
        // LEA, modr/m
        if self.ram[address.0 as usize] == 0b10001101 {
            instruction_size += 1;
            let modrm = Self::parse_modrm(&OperandSize::Word, &self.ram[address.0 as usize + 1]);
            if modrm.0 == Mod::Register {
                return Err("Invalid instruction".try_into().unwrap());
            }
            instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
            let address = self.calculate_offset_by_modrm(&address, modrm.0, modrm.2);
            let register = RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap());
            self.cpu.write_register(&register, &Value::Word(address));
        }
        // LDS, modr/m
        if self.ram[address.0 as usize] == 0b11000101 {
            instruction_size += 1;
            let modrm = Self::parse_modrm(&OperandSize::Word, &self.ram[address.0 as usize + 1]);
            if modrm.0 == Mod::Register {
                return Err("Invalid instruction".try_into().unwrap());
            }
            instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
            let address = self.calculate_address_by_modrm(&address, modrm.0, modrm.2, &segment_override);
            let register = RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap());
            let segment = RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DS);
            let value = self.read_dword(&address);
            self.cpu.write_register(&register, &Value::Word(value as u16));
            self.cpu.write_register(&segment, &Value::Word((value >> 16) as u16));
        }
        // LES, modr/m
        if self.ram[address.0 as usize] == 0b11000100 {
            instruction_size += 1;
            let modrm = Self::parse_modrm(&OperandSize::Word, &self.ram[address.0 as usize + 1]);
            if modrm.0 == Mod::Register {
                return Err("Invalid instruction".try_into().unwrap());
            }
            instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
            let address = self.calculate_address_by_modrm(&address, modrm.0, modrm.2, &segment_override);
            let register = RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap());
            let segment = RegisterEncoding::RegisterEncoding16(RegisterEncoding16::ES);
            let value = self.read_dword(&address);
            self.cpu.write_register(&register, &Value::Word(value as u16));
            self.cpu.write_register(&segment, &Value::Word((value >> 16) as u16));
        }
        // LAHF
        if self.ram[address.0 as usize] == 0b10011111 {
            let value = Value::Byte(((self.cpu.flags as u8) & 0b11010101) | 0b00000010);
            self.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH), &value);
        }
        // SAHF
        if self.ram[address.0 as usize] == 0b10011110 {
            let mut value = self.cpu.read_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH));
            value = match value {
                Value::Byte(b) => Value::Byte(b | 0b00000010),
                _ => unreachable!(),
            };
            match value {
                Value::Byte(b) => self.cpu.flags = (self.cpu.flags & 0xFF00) | b as u16,
                _ => unreachable!(),
            };
        }
        // PUSHF
        if self.ram[address.0 as usize] == 0b10011100 {
            self.push_word(self.cpu.flags);
        }
        // POPF
        if self.ram[address.0 as usize] == 0b10011101 {
            let value = self.pop_word();
            self.cpu.flags = value;
        }
        // CBW
        if self.ram[address.0 as usize] == 0b10011000 {
            let register = RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL);
            let register_value = self.cpu.read_register(&register);
            let result = match register_value {
                Value::Byte(b) => Value::Word(sign_extend(b)),
                _ => unreachable!(),
            };
            self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX), &result);
        }
        // CWD
        if self.ram[address.0 as usize] == 0b10011001 {
            let register = RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX);
            let register_value = self.cpu.read_register(&register);
            let result = match register_value {
                Value::Word(w) => Value::Word((sign_extend_16(w) >> 16) as u16),
                _ => unreachable!(),
            };
            self.cpu.write_register(&RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX), &result);
        }
        // DAA
        if self.ram[address.0 as usize] == 0b00100111 {
            // TODO
        }
        // DAS
        if self.ram[address.0 as usize] == 0b00101111 {
            // TODO
        }
        // AAA
        if self.ram[address.0 as usize] == 0b00110111 {
            // TODO
        }
        // AAS
        if self.ram[address.0 as usize] == 0b00111111 {
            // TODO
        }
        // AAM
        if self.ram[address.0 as usize] == 0b11010100 && self.ram[address.0 as usize + 1] == 0b00001010 {
            // TODO
        }
        // AAD
        if self.ram[address.0 as usize] == 0b11010101 && self.ram[address.0 as usize + 1] == 0b00001010 {
            // TODO
        }
        self.cpu.ip += instruction_size;
        Ok(())
    }

    fn read_port(&self, port: u16, operand_size: &OperandSize) -> Value {
        match operand_size {
            // TODO
            OperandSize::Byte => Value::Byte(0),
            OperandSize::Word => Value::Word(0),
        }
    }

    fn write_port(&self, port: u16, value: Value) {
        match value {
            // TODO
            Value::Byte(b) => (),
            Value::Word(w) => (),
            _ => unreachable!(),
        }
    }

    fn update_flags(&mut self, flags: &str, before: Option<Value>, after: Option<Value>, increasing: Option<bool>) {
        if flags.contains("C") {
            self.update_carry_flag(before.as_ref().unwrap(), after.as_ref().unwrap());
        }
        if flags.contains("Z") {
            self.update_zero_flag(after.as_ref().unwrap());
        }
        if flags.contains("S") {
            self.update_sign_flag(after.as_ref().unwrap());
        }
        if flags.contains("O") {
            self.update_overflow_flag(before.as_ref().unwrap(), after.as_ref().unwrap(), increasing.as_ref().unwrap())
        }
        if flags.contains("P") {
            self.update_parity_flag(after.as_ref().unwrap());
        }
        if flags.contains("A") {
            self.update_aux_carry_flag(before.as_ref().unwrap(), after.as_ref().unwrap());
        }
    }

    fn update_carry_flag(&mut self, before: &Value, after: &Value) {
        let msb_before = match before {
            Value::Byte(b) => b >> 7,
            Value::Word(w) => (w >> 15) as u8,
            _ => unreachable!(),
        };
        let msb_after = match after {
            Value::Byte(b) => b >> 7,
            Value::Word(w) => (w >> 15) as u8,
            _ => unreachable!(),
        };
        if msb_before != msb_after {
            self.cpu.flags |= Flags::CF.bits();
        } else {
            self.cpu.flags &= !Flags::CF.bits();
        }
    }

    fn update_parity_flag(&mut self, after: &Value) {
        let lsb = match after {
            Value::Byte(b) => *b,
            Value::Word(w) => *w as u8,
            _ => unreachable!(),
        };
        let parity = lsb.count_ones() % 2 == 0;
        if parity  {
            self.cpu.flags |= Flags::PF.bits();
        } else {
            self.cpu.flags &= !Flags::PF.bits();
        }
    }

    fn update_aux_carry_flag(&mut self, before: &Value, after: &Value) {
        let third_bit_before = match before {
            Value::Byte(b) => (b >> 3) & 0b00000001,
            Value::Word(w) => ((w >> 3) as u8) & 0b00000001,
            _ => unreachable!(),
        };
        let third_bit_after = match after {
            Value::Byte(b) => (b >> 3) & 0b00000001,
            Value::Word(w) => ((w >> 3) as u8) & 0b00000001,
            _ => unreachable!(),
        };
        if third_bit_before != third_bit_after {
            self.cpu.flags |= Flags::AF.bits();
        } else {
            self.cpu.flags &= !Flags::AF.bits();
        }
    }

    fn update_zero_flag(&mut self, after: &Value) {
        let is_zero = match after {
            Value::Byte(b) => *b == 0,
            Value::Word(w) => *w == 0,
            _ => unreachable!(),
        };
        if is_zero {
            self.cpu.flags |= Flags::ZF.bits();
        } else {
            self.cpu.flags &= !Flags::ZF.bits();
        }
    }

    fn update_sign_flag(&mut self, after: &Value) {
        let sign = match after {
            Value::Byte(b) => (b >> 7) == 1,
            Value::Word(w) => ((w >> 15) as u8) == 1,
            _ => unreachable!(),
        };
        if sign {
            self.cpu.flags |= Flags::SF.bits();
        } else {
            self.cpu.flags &= !Flags::SF.bits();
        }
    }

    fn update_overflow_flag(&mut self, before: &Value, after: &Value, increasing: &bool) {
        let value_before = match before {
            Value::Byte(b) => *b as u16,
            Value::Word(w) => *w,
            _ => unreachable!(),
        };
        let value_after = match after {
            Value::Byte(b) => *b as u16,
            Value::Word(w) => *w,
            _ => unreachable!(),
        };
        // TODO: What if overflows and becomes the same value?
        let overflow = (*increasing && value_before > value_after) || (!*increasing && value_before < value_after);
        if overflow {
            self.cpu.flags |= Flags::OF.bits();
        } else {
            self.cpu.flags &= !Flags::OF.bits();
        }
    }

    pub fn read_word(&self, m: &U20) -> u16 {
        self.ram[m.0 as usize] as u16 | ((self.ram[(m.0 + 1) as usize] as u16) << 8)
    }

    pub fn read_dword(&self, m: &U20) -> u32 {
        let second_word_address = U20::new_from_address(m.0 + 2);
        self.read_word(m) as u32 | ((self.read_word(&second_word_address) as u32) << 16)
    }

    pub fn write_word(&mut self, m: &U20, w: u16) {
        self.ram[m.0 as usize] = w as u8;
        self.ram[(m.0 + 1) as usize] = (w >> 8) as u8
    }

    pub fn write_mem(&mut self, address: &U20, value: &Value) {
        match value {
            Value::Byte(b) => {
                self.ram[address.0 as usize] = *b
            },
            Value::Word(w) => {
                self.write_word(&address, *w);
            },
            _ => unreachable!(),
        };
    }

    fn write_operand(&mut self, operand: &Operand, value: &Value) {
        match operand {
            Operand::Register(r) => {
                self.cpu.write_register(&r, &value);
            },
            Operand::Memory(m) => {
                self.write_mem(&m, &value);
            },
        };
    }

    fn pop_word_into_operand(&mut self, operand: &Operand) {
        let value = self.pop_word();
        match operand {
            Operand::Register(r) => {
                self.cpu.mutate_register(&r, |r: &mut u16, _h: RegisterHalf| {
                    *r = value;
                });
            },
            Operand::Memory(m) => {
                self.write_word(&m, value);
            },
        };
    }

    fn pop_word(&mut self) -> u16 {
        let address = U20::new(self.cpu.ss, self.cpu.sp);
        let value = self.ram[address.0 as usize] as u16 | (self.ram[address.0 as usize + 1] as u16) << 8;
        self.cpu.sp += 2;
        value
    }

    fn get_operand_value(&self, operand: &Operand, operand_size: &OperandSize) -> Value {
        match operand {
            Operand::Register(ref r) => self.cpu.read_register(&r),
            Operand::Memory(ref m) => {
                match operand_size {
                    OperandSize::Byte => Value::Byte(self.ram[m.0 as usize]),
                    OperandSize::Word => Value::Word(self.read_word(m)),
                }
            }
        }
    }

    fn push_word(&mut self, value: u16) {
        let address = U20::new(self.cpu.ss, self.cpu.sp);
        self.ram[address.0 as usize - 1] = value.bits(8..16) as u8;
        self.ram[address.0 as usize - 2] = value.bits(0..8) as u8;
        self.cpu.sp -= 2;
    }

    fn inc_register(&mut self, register: &RegisterEncoding) {
        self.cpu.mutate_register(&register, |r: &mut u16, h: RegisterHalf| {
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

    fn dec_register(&mut self, register: &RegisterEncoding) {
        self.cpu.mutate_register(&register, |r: &mut u16, h: RegisterHalf| {
            match h {
                RegisterHalf::FULL => *r = *r - 1,
                RegisterHalf::HIGH => {
                    let mut high = *r >> 8;
                    high -= 1;
                    high = high << 8 & 0b1111111100000000;
                    let low = *r & 0b0000000011111111;
                    *r = high | low;
                },
                RegisterHalf::LOW => {
                    let mut low = *r & 0b0000000011111111;
                    low -= 1;
                    low = low & 0b0000000011111111;
                    let high = *r & 0b1111111100000000;
                    *r = high | low;
                },
            }
        });
    }
}

#[derive(Debug)]
pub struct U20(pub u32);

impl U20 {
    pub fn new(segment: u16, offset: u16) -> Self {
        U20(u32::from(segment) * 16 + u32::from(offset))
    }

    fn new_from_address(address: u32) -> Self {
        U20(address)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inc_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // inc bp
        disk[0] = 0b01000101;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 1);
            }
        }
    }

    #[test]
    fn test_inc_register_modrm() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // inc cx (with modrm)
        disk[0] = 0b11111111;
        disk[1] = 0b11000001;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.cx, 1);
            }
        }
    }

    #[test]
    fn test_inc_register_modrm_lower_half_assert_no_overflow_to_high_half() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // inc bl
        disk[0] = 0b11111110;
        disk[1] = 0b11000011;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.bx = 0b0000000011111111;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bx, 0);
            }
        }
    }

    #[test]
    fn test_inc_byte_one_displacement() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // inc byte [bx+si+0x5c]
        disk[0] = 0b11111110;
        disk[1] = 0b01000000;
        disk[2] = 0b01011100;
        // hlt
        disk[3] = 0xF4;

        let mut emulator = Emulator::new(disk);
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                let offset = emulator.cpu.bx + emulator.cpu.si + 0x5c;
                let address = U20::new(emulator.cpu.ds, offset);
                assert_eq!(emulator.ram[address.0 as usize], 1);
            }
        }
    }

    #[test]
    fn test_segment_override_inc_word_one_displacement() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // inc word [es:si+0x5c]
        disk[0] = 0b00100110;
        disk[1] = 0b11111111;
        disk[2] = 0b01000100;
        disk[3] = 0b01011100;
        // hlt
        disk[4] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.es = 0x666;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                let offset = emulator.cpu.si + 0x5c;
                let address = U20::new(emulator.cpu.es, offset);
                assert_eq!(emulator.ram[address.0 as usize], 1);
            }
        }
    }

    #[test]
    fn test_inc_byte_direct() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // inc byte [0x5af0]
        disk[0] = 0b11111110;
        disk[1] = 0b00000110;
        disk[2] = 0b11110000;
        disk[3] = 0b01011010;
        // hlt
        disk[4] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ds = 0x666;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                let offset = 0x5AF0;
                let address = U20::new(emulator.cpu.ds, offset);
                assert_eq!(emulator.ram[address.0 as usize], 1);
            }
        }
    }

    #[test]
    fn test_add_half_registers_register_operand_direction() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // add ch,bl
        disk[0] = 0b00000010;
        disk[1] = 0b11101011;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.bx = 0b0000000000000010;
        emulator.cpu.cx = 0b0000000100000000;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.cx, 0b0000001100000000);
            }
        }
    }

    #[test]
    fn test_inc_register_modrm_assert_overflow_flag() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // inc cx (with modrm)
        disk[0] = 0b11111111;
        disk[1] = 0b11000001;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.cx = 0xFFFF;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.cx, 0);
                assert_eq!(emulator.cpu.flags & Flags::OF.bits(), Flags::OF.bits());
            }
        }
    }

    #[test]
    fn test_mov_immediate_word() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // mov di,0xf00f
        disk[0] = 0b10111111;
        disk[1] = 0b00001111;
        disk[2] = 0b11110000;
        // hlt
        disk[3] = 0xF4;

        let mut emulator = Emulator::new(disk);
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.di, 0xF00F);
            }
        }
    }

    #[test]
    fn test_add_immediate_to_memory_sign_extend() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // add word [bx],byte -0x71
        disk[0] = 0b10000011;
        disk[1] = 0b00000111;
        disk[2] = 0b10001111;
        // hlt
        disk[3] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.bx = 0x666;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                let offset = 0x666;
                let address = U20::new(emulator.cpu.ds, offset);
                assert_eq!(emulator.ram[address.0 as usize], 0b10001111);
                assert_eq!(emulator.ram[address.0 as usize + 1], 0b11111111);
            }
        }
    }

    #[test]
    fn test_mov_modrm_register_to_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // mov di,bx
        disk[0] = 0b10001011;
        disk[1] = 0b11111011;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.di = 0xCAFE;
        emulator.cpu.bx = 0xBABE;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.di, 0xBABE);
            }
        }
    }

    #[test]
    fn test_mov_modrm_immediate_word() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // mov cx,0xaaaa
        disk[0] = 0b11000111;
        disk[1] = 0b11000001;
        disk[2] = 0b10101010;
        disk[3] = 0b10101010;
        // hlt
        disk[4] = 0xF4;

        let mut emulator = Emulator::new(disk);
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.cx, 0xAAAA);
            }
        }
    }

    #[test]
    fn test_mov_mem_to_ax() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // mov ax,[0x7ff8]
        disk[0] = 0b10100001;
        disk[1] = 0b11111000;
        disk[2] = 0b01111111;
        // hlt
        disk[3] = 0xF4;

        let mut emulator = Emulator::new(disk);
        let address = U20::new(emulator.cpu.ds, 0x7FF8);
        emulator.ram[address.0 as usize] = 0xAD;
        emulator.ram[address.0 as usize + 1] = 0xDE;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.ax, 0xDEAD);
            }
        }
    }

    #[test]
    fn test_mov_al_to_mem() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // mov [0xcccc],al
        disk[0] = 0b10100010;
        disk[1] = 0b11001100;
        disk[2] = 0b11001100;
        // hlt
        disk[3] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ax = 0xDEAD;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                let address = U20::new(emulator.cpu.ds, 0xCCCC);
                assert_eq!(emulator.ram[address.0 as usize], 0xAD);
                assert_eq!(emulator.ram[address.0 as usize + 1], 0x00);
            }
        }
    }

    #[test]
    fn test_mov_segment_to_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // mov ss,si
        disk[0] = 0b10001110;
        disk[1] = 0b11010110;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0xDEAD;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.ss, 0xDEAD);
            }
        }
    }

    #[test]
    fn test_push_modrm_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // push bx
        disk[0] = 0b11111111;
        disk[1] = 0b11110011;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ss = 0x00A8;
        emulator.cpu.sp = 0x000C;
        emulator.cpu.bx = 0xA01F;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                let address = U20::new(0x00A8, 0x000C);
                assert_eq!(emulator.ram[address.0 as usize - 1], 0xA0);
                assert_eq!(emulator.ram[address.0 as usize - 2], 0x1F);
                assert_eq!(emulator.cpu.sp, 0x000A);
            }
        }
    }

    #[test]
    fn test_push_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // push bx
        disk[0] = 0b01010011;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ss = 0x00A8;
        emulator.cpu.sp = 0x000C;
        emulator.cpu.bx = 0xA01F;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                let address = U20::new(0x00A8, 0x000C);
                assert_eq!(emulator.ram[address.0 as usize - 1], 0xA0);
                assert_eq!(emulator.ram[address.0 as usize - 2], 0x1F);
                assert_eq!(emulator.cpu.sp, 0x000A);
            }
        }
    }

    #[test]
    fn test_push_segment_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // push ds
        disk[0] = 0b00011110;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ss = 0x00A8;
        emulator.cpu.sp = 0x000C;
        emulator.cpu.ds = 0xA01F;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                let address = U20::new(0x00A8, 0x000C);
                assert_eq!(emulator.ram[address.0 as usize - 1], 0xA0);
                assert_eq!(emulator.ram[address.0 as usize - 2], 0x1F);
                assert_eq!(emulator.cpu.sp, 0x000A);
            }
        }
    }

    #[test]
    fn test_pop_modrm_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // pop bp
        disk[0] = 0b10001111;
        disk[1] = 0b11000101;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ss = 0x00A8;
        emulator.cpu.sp = 0x000A;
        let address = U20::new(0x00A8, 0x000A);
        emulator.ram[address.0 as usize] = 0x1F;
        emulator.ram[address.0 as usize + 1] = 0xA0;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 0xA01F);
                assert_eq!(emulator.cpu.sp, 0x000C)
            }
        }
    }

    #[test]
    fn test_pop_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // pop si
        disk[0] = 0b01011110;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ss = 0x00A8;
        emulator.cpu.sp = 0x000A;
        let address = U20::new(0x00A8, 0x000A);
        emulator.ram[address.0 as usize] = 0x1F;
        emulator.ram[address.0 as usize + 1] = 0xA0;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.si, 0xA01F);
                assert_eq!(emulator.cpu.sp, 0x000C)
            }
        }
    }

    #[test]
    fn test_pop_segment_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // pop ds
        disk[0] = 0b00011111;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ss = 0x00A8;
        emulator.cpu.sp = 0x000A;
        let address = U20::new(0x00A8, 0x000A);
        emulator.ram[address.0 as usize] = 0x1F;
        emulator.ram[address.0 as usize + 1] = 0xA0;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.ds, 0xA01F);
                assert_eq!(emulator.cpu.sp, 0x000C)
            }
        }
    }

    #[test]
    fn test_xchg_modrm() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // xchg dh,bh
        disk[0] = 0b10000110;
        disk[1] = 0b11110111;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.dx = 0xFFFF;
        emulator.cpu.bx = 0xAAAA;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.dx, 0xAAFF);
                assert_eq!(emulator.cpu.bx, 0xFFAA);
            }
        }
    }

    #[test]
    fn test_xchg_ax() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // xchg ax,bp
        disk[0] = 0b10010101;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ax = 0xFFFF;
        emulator.cpu.bp = 0xAAAA;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 0xFFFF);
                assert_eq!(emulator.cpu.ax, 0xAAAA);
            }
        }
    }

    #[test]
    fn test_xlat() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // xlat
        disk[0] = 0b11010111;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ax = 0b0000000000000010;
        emulator.cpu.bx = 0x00AA;
        emulator.ram[0x00AA] = 0xFF;
        emulator.ram[0x00AB] = 0xFF;
        emulator.ram[0x00AC] = 0xFA;
        emulator.ram[0x00AD] = 0xFF;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.ax, 0x00FA);
            }
        }
    }

    #[test]
    fn test_lea_direct() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // lea bp,[0xf0f0]
        disk[0] = 0b10001101;
        disk[1] = 0b00101110;
        disk[2] = 0b11110000;
        disk[3] = 0b11110000;
        // hlt
        disk[4] = 0xF4;

        let mut emulator = Emulator::new(disk);
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 0xF0F0);
            }
        }
    }

    #[test]
    fn test_lea() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // lea bp,[si+0x7070]
        disk[0] = 0x8D;
        disk[1] = 0xAC;
        disk[2] = 0x70;
        disk[3] = 0x70;
        // hlt
        disk[4] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0x01;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 0x7071);
            }
        }
    }

    #[test]
    fn test_lea_negative_displacement() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // lea bp,[si-0xf10]
        disk[0] = 0x8D;
        disk[1] = 0xAC;
        disk[2] = 0xF0;
        disk[3] = 0xF0;
        // hlt
        disk[4] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0xFFFF;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 0xF0EF);
            }
        }
    }

    #[test]
    fn test_lea_negative_displacement_overflow() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // lea bp,[si-0xf10]
        disk[0] = 0x8D;
        disk[1] = 0xAC;
        disk[2] = 0xF0;
        disk[3] = 0xF0;
        // hlt
        disk[4] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0x0F11;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 0x0001);
            }
        }
    }

    #[test]
    fn test_lds() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // lds si,[bp+di+0x5555]
        disk[0] = 0xC5;
        disk[1] = 0xB3;
        disk[2] = 0x55;
        disk[3] = 0x55;
        // hlt
        disk[4] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0x0001;
        emulator.cpu.bp = 0x0001;
        emulator.cpu.di = 0x0001;
        let address = U20::new(emulator.cpu.ds, emulator.cpu.bp + emulator.cpu.di + 0x5555);
        emulator.ram[address.0 as usize] = 0x1F;
        emulator.ram[address.0 as usize + 1] = 0x3C;
        emulator.ram[address.0 as usize + 2] = 0xA5;
        emulator.ram[address.0 as usize + 3] = 0x87;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.ds, 0x87A5);
                assert_eq!(emulator.cpu.si, 0x3C1F);
            }
        }
    }

    #[test]
    fn test_lahf() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // lahf
        disk[0] = 0b10011111;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.flags = Flags::SF.bits() | Flags::AF.bits();
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.ax, 0b1001001000000000);
            }
        }
    }

    #[test]
    fn test_sahf() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // sahf
        disk[0] = 0b10011110;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ax = 0b1001001000000000;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.flags, 0b0000000010010010);
            }
        }
    }

    #[test]
    fn test_pushf() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // pushf
        disk[0] = 0b10011100;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.sp = 2;
        emulator.cpu.flags = 0xFFFF;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.ram[0], 0xFF);
                assert_eq!(emulator.ram[1], 0xFF);
            }
        }
    }

    #[test]
    fn test_popf() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // popf
        disk[0] = 0b10011101;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.ram[0] = 0xFF;
        emulator.ram[1] = 0xFF;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.flags, 0xFFFF);
            }
        }
    }

    #[test]
    fn test_add_immediate_accumulator() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // add al,0xaa
        disk[0] = 0b00000100;
        disk[1] = 0b10101010;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ax = 0x0011;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.ax, 0x00BB);
            }
        }
    }

    #[test]
    fn test_neg_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // neg bx
        disk[0] = 0b11110111;
        disk[1] = 0b11011011;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.bx = 0xFFFF;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bx, 1);
            }
        }
    }

    #[test]
    fn test_cmp_register_to_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // cmp di,bp
        disk[0] = 0b00111001;
        disk[1] = 0b11101111;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.di = 0xFF;
        emulator.cpu.bp = 0x01;
        emulator.cpu.flags = Flags::CF.bits() | Flags::ZF.bits();
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.di, 0xFF);
                assert_eq!(emulator.cpu.bp, 0x01);
                assert_eq!(emulator.cpu.flags, 0);
            }
        }
    }

    #[test]
    fn test_jmp_short_direct_intrasegment() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // jmp short "+1"
        disk[0] = 0b11101011;
        disk[1] = 0b00000001;
        // hlt
        disk[2] = 0xF4;
        // inc bp
        disk[3] = 0b01000101;
        // hlt
        disk[4] = 0xF4;

        let mut emulator = Emulator::new(disk);
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 1);
            }
        }
    }

    #[test]
    fn test_jmp_direct_intrasegment() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // jmp "+2"
        disk[0] = 0b11101001;
        disk[1] = 0b00000010;
        disk[2] = 0b00000000;
        // hlt
        disk[3] = 0xF4;
        // inc bp
        disk[4] = 0b01000101;
        // hlt
        disk[5] = 0xF4;

        let mut emulator = Emulator::new(disk);
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 1);
            }
        }
    }

    #[test]
    fn test_jmp_direct_intersegment() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // jmp 0x2:0x2
        disk[0] = 0b11101010;
        disk[1] = 0b00000010;
        disk[2] = 0b00000000;
        disk[3] = 0b00000010;
        disk[4] = 0b00000000;
        // hlt
        disk[5] = 0xF4;

        let mut emulator = Emulator::new(disk);
        let address = U20::new(0x2, 0x2);
        // inc bp
        emulator.ram[address.0 as usize] = 0b01000101;
        // hlt
        emulator.ram[address.0 as usize + 1] = 0xF4;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 1);
            }
        }
    }

    #[test]
    fn test_jmp_indirect_intrasegment() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // jmp bx
        disk[0] = 0b11111111;
        disk[1] = 0b11100011;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.bx = 0x09;
        emulator.ram[9] = 0b01000101;
        emulator.ram[10] = 0xF4;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 1);
            }
        }
    }

    #[test]
    fn test_jmp_indirect_intersegment() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // jmp far [bx]
        disk[0] = 0b11111111;
        disk[1] = 0b00101111;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.bx = 0x09;
        emulator.ram[9] = 0b00000010;
        emulator.ram[10] = 0b00000000;
        emulator.ram[11] = 0b00000010;
        emulator.ram[12] = 0b00000000;
        let address = U20::new(0x2, 0x2);
        emulator.ram[address.0 as usize] = 0b01000101;
        emulator.ram[address.0 as usize + 1] = 0xF4;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 1);
            }
        }
    }

    #[test]
    fn test_call_indirect_intersegment() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // call far [bx]
        disk[0] = 0b11111111;
        disk[1] = 0b00011111;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ss = 0xAAAA;
        emulator.cpu.sp = 4;
        emulator.cpu.bx = 0x09;
        emulator.ram[9] = 0b00000010;
        emulator.ram[10] = 0b00000000;
        emulator.ram[11] = 0b00000010;
        emulator.ram[12] = 0b00000000;
        let address = U20::new(0x2, 0x2);
        emulator.ram[address.0 as usize] = 0b01000101;
        emulator.ram[address.0 as usize + 1] = 0xF4;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 1);
                let address = U20::new(0xAAAA, 4);
                assert_eq!(emulator.ram[address.0 as usize - 1], 0);
                assert_eq!(emulator.ram[address.0 as usize - 2], 0);
                assert_eq!(emulator.ram[address.0 as usize - 3], 0x7c);
                assert_eq!(emulator.ram[address.0 as usize - 4], 0x02);
            }
        }
    }

    #[test]
    fn test_shl_1_bit_word_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // shl si,1
        disk[0] = 0b11010001;
        disk[1] = 0b11100110;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0b1010101010101010;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.si, 0b0101010101010100);
                assert_eq!((emulator.cpu.flags & Flags::CF.bits()).count_ones(), 1);
                assert_eq!((emulator.cpu.flags & Flags::OF.bits()).count_ones(), 1);
            }
        }
    }

    #[test]
    fn test_shr_2_bits_byte_memory() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // shr byte [di],cl
        disk[0] = 0b11010010;
        disk[1] = 0b00101101;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0b1010101010101010;
        emulator.cpu.cx = 2;
        emulator.cpu.di = 23;
        let address = U20::new(0, 23);
        emulator.ram[address.0 as usize] = 0b01010111;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.ram[address.0 as usize], 0b00010101);
                assert_eq!((emulator.cpu.flags & Flags::CF.bits()).count_ones(), 1);
            }
        }
    }

    #[test]
    fn test_sar_1_bit_byte_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // sar ch,1
        disk[0] = 0b11010000;
        disk[1] = 0b11111101;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.cx = 0b1010101000000000;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.cx, 0b1101010100000000);
                assert_eq!((emulator.cpu.flags & Flags::CF.bits()).count_ones(), 0);
            }
        }
    }

    #[test]
    fn test_rol_1_bit_word_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // rol si,1
        disk[0] = 0b11010001;
        disk[1] = 0b11000110;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0b1010000000000000;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.si, 0b0100000000000001);
                assert_eq!((emulator.cpu.flags & Flags::CF.bits()).count_ones(), 1);
                assert_eq!((emulator.cpu.flags & Flags::OF.bits()).count_ones(), 1);
            }
        }
    }

    #[test]
    fn test_rcl_2_bits_byte_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // rcl dh,cl
        disk[0] = 0b11010010;
        disk[1] = 0b11010110;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.cx = 2;
        emulator.cpu.dx = 0b1010101000000000;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.dx, 0b1010100100000000);
                assert_eq!((emulator.cpu.flags & Flags::CF.bits()).count_ones(), 0);
            }
        }
    }

    #[test]
    fn test_rcr_3_bits_byte_register() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // rcr dh,cl
        disk[0] = 0b11010010;
        disk[1] = 0b11011110;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.cx = 3;
        emulator.cpu.dx = 0b1010101000000000;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.dx, 0b1001010100000000);
                assert_eq!((emulator.cpu.flags & Flags::CF.bits()).count_ones(), 0);
            }
        }
    }

    #[test]
    fn test_mul_word() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // mul bp
        disk[0] = 0b11110111;
        disk[1] = 0b11100101;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.bp = 10000;
        emulator.cpu.ax = 10000;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.dx, 0b0000010111110101);
                assert_eq!(emulator.cpu.ax, 0b1110000100000000);
                assert_eq!((emulator.cpu.flags & Flags::CF.bits()).count_ones(), 1);
                assert_eq!((emulator.cpu.flags & Flags::OF.bits()).count_ones(), 1);
            }
        }
    }

    #[test]
    fn test_imul_word() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // imul bp
        disk[0] = 0b11110111;
        disk[1] = 0b11101101;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.bp = (-1 as i16) as u16;
        emulator.cpu.ax = 2;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.dx, 65535);
                assert_eq!(emulator.cpu.ax, 65534);
            }
        }
    }

    #[test]
    fn test_div_word() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // div bp
        disk[0] = 0b11110111;
        disk[1] = 0b11110101;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.bp = 3;
        emulator.cpu.dx = 0;
        emulator.cpu.ax = 10;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.ax, 3);
                assert_eq!(emulator.cpu.dx, 1);
            }
        }
    }

    #[test]
    fn test_idiv_byte() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // idiv ch
        disk[0] = 0b11110110;
        disk[1] = 0b11111101;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.cx = 0b0000001000000000; // 2
        emulator.cpu.ax = 0b1111111111111101; // -3
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.ax, 0b1111111111111111);
            }
        }
    }

    #[test]
    fn test_loop() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // inc bp
        disk[0] = 0b01000101;
        // loop "-1"
        disk[1] = 0b11100010;
        disk[2] = 0xFD;
        // hlt
        disk[3] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.cx = 3;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.bp, 3);
            }
        }
    }

    #[test]
    fn test_or_displacement_and_immediate() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // or byte [es:di+0x20],0x20
        disk[0] = 0b00100110;
        disk[1] = 0b10000000;
        disk[2] = 0b01001101;
        disk[3] = 0b00100000;
        disk[4] = 0b00100000;
        // hlt
        disk[5] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.es = 1;
        emulator.cpu.di = 1;
        let address = U20::new(emulator.cpu.es, emulator.cpu.di + 0x20);
        emulator.ram[address.0 as usize] = 0x10;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.ram[address.0 as usize], 0x10 | 0x20);
            }
        }
    }

    #[test]
    fn test_interrupt() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // int 0x11
        disk[0] = 0xCD;
        disk[1] = 0x11;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.ss = 0x0F00;
        let run = emulator.run(false);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.ax, 0b0000000000110001);
            }
        }
    }

    #[test]
    fn test_movs_word() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // movsw
        disk[0] = 0b10100101;
        // hlt
        disk[1] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0x5;
        emulator.cpu.di = 0x3;
        emulator.cpu.ds = 0x10;
        emulator.cpu.es = 0x30;
        let source_addr = U20::new(emulator.cpu.ds, emulator.cpu.si);
        let dest_addr = U20::new(emulator.cpu.es, emulator.cpu.di);
        emulator.ram[source_addr.0 as usize] = 0xBA;
        emulator.ram[source_addr.0 as usize + 1] = 0xBE;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.si, 0x5 + 2);
                assert_eq!(emulator.cpu.di, 0x3 + 2);
                assert_eq!(emulator.ram[dest_addr.0 as usize], 0xBA);
                assert_eq!(emulator.ram[dest_addr.0 as usize + 1], 0xBE);
            }
        }
    }

    #[test]
    fn test_movs_byte_rep() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // rep movsb
        disk[0] = 0b11110011;
        disk[1] = 0b10100100;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0x5;
        emulator.cpu.di = 0x3;
        emulator.cpu.ds = 0x10;
        emulator.cpu.es = 0x30;
        emulator.cpu.cx = 0x3;
        let source_addr = U20::new(emulator.cpu.ds, emulator.cpu.si);
        let dest_addr = U20::new(emulator.cpu.es, emulator.cpu.di);
        emulator.ram[source_addr.0 as usize] = 0xBA;
        emulator.ram[source_addr.0 as usize + 1] = 0xBE;
        emulator.ram[source_addr.0 as usize + 2] = 0xCA;
        emulator.ram[source_addr.0 as usize + 3] = 0xFE;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.si, 0x5 + 3);
                assert_eq!(emulator.cpu.di, 0x3 + 3);
                assert_eq!(emulator.cpu.cx, 0);
                assert_eq!(emulator.ram[dest_addr.0 as usize], 0xBA);
                assert_eq!(emulator.ram[dest_addr.0 as usize + 1], 0xBE);
                assert_eq!(emulator.ram[dest_addr.0 as usize + 2], 0xCA);
                assert_eq!(emulator.ram[dest_addr.0 as usize + 3], 0x0);
            }
        }
    }

    #[test]
    fn test_cmps_byte_repne() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // repne cmpsb
        disk[0] = 0xF2;
        disk[1] = 0xA6;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0x5;
        emulator.cpu.di = 0x3;
        emulator.cpu.ds = 0x10;
        emulator.cpu.es = 0x30;
        emulator.cpu.cx = 0x3;
        let source_addr = U20::new(emulator.cpu.ds, emulator.cpu.si);
        let dest_addr = U20::new(emulator.cpu.es, emulator.cpu.di);
        emulator.ram[source_addr.0 as usize] = 0xBA;
        emulator.ram[source_addr.0 as usize + 1] = 0xBE;
        emulator.ram[source_addr.0 as usize + 2] = 0xCA;
        emulator.ram[source_addr.0 as usize + 3] = 0xFE;
        emulator.ram[dest_addr.0 as usize] = 0xBA;
        emulator.ram[dest_addr.0 as usize + 1] = 0xBE;
        emulator.ram[dest_addr.0 as usize + 2] = 0xCA;
        emulator.ram[dest_addr.0 as usize + 3] = 0xFE;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.si, 0x5 + 1);
                assert_eq!(emulator.cpu.di, 0x3 + 1);
                assert_eq!(emulator.cpu.cx, 2);
            }
        }
    }

    #[test]
    fn test_cmps_byte_repe() {
        let mut disk = vec![0; 1024 * 1024 * 50].into_boxed_slice();

        disk[510] = 0x55;
        disk[511] = 0xAA;

        // repe cmpsb
        disk[0] = 0xF3;
        disk[1] = 0xA6;
        // hlt
        disk[2] = 0xF4;

        let mut emulator = Emulator::new(disk);
        emulator.cpu.si = 0x5;
        emulator.cpu.di = 0x3;
        emulator.cpu.ds = 0x10;
        emulator.cpu.es = 0x30;
        emulator.cpu.cx = 0x3;
        let source_addr = U20::new(emulator.cpu.ds, emulator.cpu.si);
        let dest_addr = U20::new(emulator.cpu.es, emulator.cpu.di);
        emulator.ram[source_addr.0 as usize] = 0xBA;
        emulator.ram[source_addr.0 as usize + 1] = 0xBE;
        emulator.ram[source_addr.0 as usize + 2] = 0xCA;
        emulator.ram[source_addr.0 as usize + 3] = 0xFE;
        emulator.ram[dest_addr.0 as usize] = 0xBA;
        emulator.ram[dest_addr.0 as usize + 1] = 0xBE;
        emulator.ram[dest_addr.0 as usize + 2] = 0xCA;
        emulator.ram[dest_addr.0 as usize + 3] = 0xFE;
        let run = emulator.run(true);

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.si, 0x5 + 3);
                assert_eq!(emulator.cpu.di, 0x3 + 3);
                assert_eq!(emulator.cpu.cx, 0);
            }
        }
    }
}
