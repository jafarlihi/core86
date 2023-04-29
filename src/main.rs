use std::convert::TryFrom;
use bitflags::bitflags;
use num_enum::TryFromPrimitive;
use intbits::Bits;

enum RegisterEncoding {
    RegisterEncoding8(RegisterEncoding8),
    RegisterEncoding16(RegisterEncoding16),
}

enum RM {
    Register(RegisterEncoding),
    BaseIndex((Option<RegisterEncoding16>, Option<RegisterEncoding16>)),
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
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

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
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

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
enum SegmentRegister {
    ES = 00,
    CS = 01,
    SS = 10,
    DS = 11,
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
    flags: u16,
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

    fn get_register(&self, register: &RegisterEncoding) -> Value {
        match register {
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::AX) => Value::Word(self.ax),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::BX) => Value::Word(self.bx),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::CX) => Value::Word(self.cx),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DX) => Value::Word(self.dx),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::SI) => Value::Word(self.si),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::DI) => Value::Word(self.di),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::BP) => Value::Word(self.bp),
            RegisterEncoding::RegisterEncoding16(RegisterEncoding16::SP) => Value::Word(self.sp),
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
}

#[derive(Debug)]
enum Value {
    Byte(u8),
    Word(u16),
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

struct Emulator {
    cpu: CPU,
    ram: Box<[u8]>,
    disk: Box<[u8]>,
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
enum ModRMMod {
    NoDisplacement = 0b00,
    OneByteDisplacement = 0b01,
    TwoByteDisplacement = 0b10,
    Register = 0b11,
    Direct = 0xFF,
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
enum TwoOperandDirection {
    ModRM = 0,
    Register = 1,
}

impl TryFrom<bool> for TwoOperandDirection {
    type Error = ();
    fn try_from(v: bool) -> Result<Self, Self::Error> {
        match v {
            x if x == true => Ok(TwoOperandDirection::Register),
            x if x == false => Ok(TwoOperandDirection::ModRM),
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

enum Operand {
    Register(RegisterEncoding),
    Memory(U20),
}

impl Emulator {
    fn new(disk: Box<[u8]>) -> Self {
        Emulator {
            cpu: CPU::new(),
            ram: vec![0; 1024 * 1024].into_boxed_slice(),
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

    fn parse_modrm(operand_size: &OperandSize, modrm: &u8) -> (ModRMMod, u8, RM) {
        let mut m: ModRMMod = (modrm >> 6).try_into().unwrap();
        let opcode = (modrm & 0b00111000) >> 3;
        let rm = match m {
            ModRMMod::Register => {
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
        if modrm & 0b00000111 == 0b00000110 && m == ModRMMod::NoDisplacement {
            m = ModRMMod::Direct;
        }
        (m, opcode, rm)
    }

    fn calculate_address_by_rm(&self, rm: RM, displacement: u16, segment_override: &Option<SegmentRegister>) -> U20 {
        let mut offset: u16 = displacement;
        let mut use_ss = false;
        offset = offset + match rm {
            RM::BaseIndex(bi) => {
                bi.0.map_or(
                    0,
                    |r| {
                        if r == RegisterEncoding16::BP {
                            use_ss = true;
                        }
                        match self.cpu.get_register(&RegisterEncoding::RegisterEncoding16(r)) {
                            Value::Word(w) => w,
                            _ => unreachable!(),
                        }
                    }
                ) + bi.1.map_or(
                    0,
                    |r| {
                        match self.cpu.get_register(&RegisterEncoding::RegisterEncoding16(r)) {
                            Value::Word(w) => w,
                            _ => unreachable!(),
                        }
                    }
                )
            }
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

    fn get_instruction_size_extension_by_mod(modrmmod: &ModRMMod) -> u16 {
        match modrmmod {
            ModRMMod::Register => 0,
            ModRMMod::NoDisplacement => 0,
            ModRMMod::OneByteDisplacement => 1,
            ModRMMod::TwoByteDisplacement => 2,
            ModRMMod::Direct => 2,
        }
    }

    fn calculate_address_by_modrm(&self, instruction_address: &U20, modrmmod: ModRMMod, rm: RM, segment_override: &Option<SegmentRegister>) -> U20 {
        match modrmmod {
            ModRMMod::OneByteDisplacement => {
                let displacement = self.ram[instruction_address.0 as usize + 2] as u16;
                self.calculate_address_by_rm(rm, displacement, segment_override)
            },
            ModRMMod::TwoByteDisplacement => {
                let displacement = self.ram[
                    instruction_address.0 as usize + 2
                ] as u16 | (self.ram[
                    instruction_address.0 as usize + 3]
                as u16) << 8;
                self.calculate_address_by_rm(rm, displacement, segment_override)
            },
            ModRMMod::NoDisplacement => {
                self.calculate_address_by_rm(rm, 0, segment_override)
            },
            ModRMMod::Direct => {
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

    fn execute(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let mut address = U20::new(self.cpu.cs, self.cpu.ip);
        if self.ram[address.0 as usize] == 0b11110100 {
            return Err("Halted".into());
        }
        let mut instruction_size = 1;
        let mut segment_override: Option<SegmentRegister> = None;
        // Segment override prefix
        if self.ram[address.0 as usize] >> 5 == 0b00000001 && self.ram[address.0 as usize] & 0b00000111 == 0b00000110 {
            instruction_size += 1;
            segment_override = Some(((self.ram[address.0 as usize] & 0b00011000) >> 3).try_into().unwrap());
            address = U20::new(self.cpu.cs, self.cpu.ip + 1);
        }
        match self.ram[address.0 as usize] >> 4 {
            0b1011 => {
                // MOV, immediate, register-mode
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
                self.cpu.mutate_register(&register, |r: &mut u16, h: RegisterHalf| {
                    match h {
                        RegisterHalf::FULL => *r = match immediate {
                            Value::Word(w) => w,
                            _ => unreachable!(),
                        },
                        RegisterHalf::HIGH => *r = match immediate {
                            Value::Byte(b) => {
                                let low = *r & 0x00FF;
                                low & ((b as u16) << 8)
                            },
                            _ => unreachable!(),
                        },
                        RegisterHalf::LOW => *r = match immediate {
                            Value::Byte(b) => {
                                let high = *r & 0xFF00;
                                high & (b as u16)
                            },
                            _ => unreachable!(),
                        },
                    }
                });
            },
            _ => (),
        }
        match self.ram[address.0 as usize] >> 3 {
            // INC, register-mode
            0b01000 => {
                let register = RegisterEncoding::RegisterEncoding16((self.ram[address.0 as usize] & 0b00000111).try_into().unwrap());
                let before = self.cpu.get_register(&register);
                self.inc_register(&register);
                let after = self.cpu.get_register(&register);
                self.update_flags("ZSOPA", Some(before), Some(after), Some(true))
            },
            _ => (),
        }
        match self.ram[address.0 as usize] >> 2 {
            // ADD, modr/m
            0b00000000 => {
                instruction_size += 1;
                let direction: TwoOperandDirection = ((self.ram[address.0 as usize] & 0b00000010) >> 1).try_into().unwrap();
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let register_value = self.cpu.get_register(&register);
                let operand = match modrm.0 {
                    ModRMMod::Register => {
                        match modrm.2 {
                            RM::Register(e) => Operand::Register(e),
                            _ => unreachable!(),
                        }
                    },
                    _ => {
                        Operand::Memory(self.calculate_address_by_modrm(&address, modrm.0, modrm.2, &segment_override))
                    },
                };
                let operand_value = match operand {
                    Operand::Register(ref r) => self.cpu.get_register(&r),
                    Operand::Memory(ref m) => {
                        match operand_size {
                            OperandSize::Byte => Value::Byte(self.ram[m.0 as usize]),
                            OperandSize::Word => Value::Word(self.read_word(m)),
                        }
                    }
                };
                let sum: Value = match register_value {
                    Value::Byte(b) => (b + match operand_value {
                        Value::Byte(b2) => b2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                    Value::Word(w) => (w + match operand_value {
                        Value::Word(w2) => w2,
                        _ => unreachable!(),
                    }).try_into().unwrap(),
                };
                match direction {
                    TwoOperandDirection::Register => {
                        self.cpu.mutate_register(&register, |r: &mut u16, h: RegisterHalf| {
                            match sum {
                                Value::Byte(b) => {
                                    match h {
                                        RegisterHalf::HIGH => *r = (*r & 0x00FF) | ((b as u16) << 8),
                                        _ => *r = b as u16,
                                    };
                                },
                                Value::Word(w) => {
                                    *r = w
                                },
                            };
                        });
                    },
                    TwoOperandDirection::ModRM => {
                        match operand {
                            Operand::Register(r) => {
                                self.cpu.mutate_register(&r, |r: &mut u16, h: RegisterHalf| {
                                   match sum {
                                       Value::Byte(b) => {
                                            match h {
                                                RegisterHalf::HIGH => *r = (*r & 0x00FF) | ((b as u16) << 8),
                                                _ => *r = b as u16,
                                            };
                                       },
                                       Value::Word(w) => {
                                           *r = w
                                       },
                                   };
                                });
                            },
                            Operand::Memory(m) => {
                                match sum {
                                    Value::Byte(b) => {
                                        self.ram[m.0 as usize] = b
                                    },
                                    Value::Word(w) => {
                                        self.write_word(&m, w);
                                    },
                                };
                            },
                        };
                    },
                }
                self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(true));
            },
            0b00100010 => {
                // MOV, mod/rm
                instruction_size += 1;
                let direction: TwoOperandDirection = self.ram[address.0 as usize].bit(1).try_into().unwrap();
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                let register: RegisterEncoding = match operand_size {
                    OperandSize::Byte => RegisterEncoding::RegisterEncoding8(modrm.1.try_into().unwrap()),
                    OperandSize::Word => RegisterEncoding::RegisterEncoding16(modrm.1.try_into().unwrap()),
                };
                let operand = match modrm.0 {
                    ModRMMod::Register => {
                        match modrm.2 {
                            RM::Register(e) => Operand::Register(e),
                            _ => unreachable!(),
                        }
                    },
                    _ => {
                        Operand::Memory(self.calculate_address_by_modrm(&address, modrm.0, modrm.2, &segment_override))
                    },
                };
                let operand_value = match operand {
                    Operand::Register(ref r) => self.cpu.get_register(&r),
                    Operand::Memory(ref m) => {
                        match operand_size {
                            OperandSize::Byte => Value::Byte(self.ram[m.0 as usize]),
                            OperandSize::Word => Value::Word(self.read_word(&m)),
                        }
                    },
                };
                let register_value = self.cpu.get_register(&register);
                match direction {
                    TwoOperandDirection::Register => {
                        self.cpu.mutate_register(&register, |r: &mut u16, h: RegisterHalf| {
                            match h {
                                RegisterHalf::FULL => {
                                    match operand_value {
                                        Value::Word(w) => *r = w,
                                        _ => unreachable!(),
                                    };
                                },
                                RegisterHalf::LOW => {
                                    match operand_value {
                                        Value::Byte(b) => {
                                            let high = *r & 0xFF00;
                                            *r = high & (b as u16);
                                        },
                                        _ => unreachable!(),
                                    }
                                },
                                RegisterHalf::HIGH => {
                                    match operand_value {
                                        Value::Byte(b) => {
                                            let low = *r & 0x00FF;
                                            *r = low & ((b as u16) << 8);
                                        },
                                        _ => unreachable!(),
                                    };
                                },
                            }
                        });
                    },
                    TwoOperandDirection::ModRM => {
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
                        };
                    },
                };
            },
            0b00100000 => {
                let mut sign_extend = self.ram[address.0 as usize].bit(1);
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                match operand_size {
                    OperandSize::Byte => sign_extend = false,
                    _ => (),
                };
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                match modrm.1 {
                    // ADD, modr/m, immediate
                    0b00000000 => {
                        let operand = match modrm.0 {
                            ModRMMod::Register => {
                                match modrm.2 {
                                    RM::Register(e) => Operand::Register(e),
                                    _ => unreachable!(),
                                }
                            },
                            _ => {
                                Operand::Memory(self.calculate_address_by_modrm(&address, modrm.0, modrm.2, &segment_override))
                            },
                        };
                        let immediate = match operand_size {
                            OperandSize::Byte => {
                                instruction_size += 1;
                                Value::Byte(self.ram[address.0 as usize + 1])
                            },
                            OperandSize::Word => {
                                if !sign_extend {
                                    instruction_size += 2;
                                    Value::Word((self.ram[address.0 as usize + 3] as u16) << 8 | self.ram[address.0 as usize + 2] as u16)
                                } else {
                                    instruction_size += 1;
                                    Value::Word(crate::sign_extend(self.ram[address.0 as usize + 2]))
                                }
                            },
                        };
                        let operand_value = match operand {
                            Operand::Register(ref r) => self.cpu.get_register(&r),
                            Operand::Memory(ref m) => {
                                match operand_size {
                                    OperandSize::Byte => Value::Byte(self.ram[m.0 as usize]),
                                    OperandSize::Word => Value::Word(self.read_word(&m)),
                                }
                            },
                        };
                        let sum: Value = match immediate {
                            Value::Byte(b) => (b + match operand_value {
                                Value::Byte(b2) => b2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                            Value::Word(w) => (w + match operand_value {
                                Value::Word(w2) => w2,
                                _ => unreachable!(),
                            }).try_into().unwrap(),
                        };
                        match sum {
                            Value::Byte(b) => {
                                match operand {
                                    Operand::Register(r) => {
                                        self.cpu.mutate_register(&r, |r: &mut u16, h: RegisterHalf| {
                                            match h {
                                                // TODO: Don't repeat this every time
                                                RegisterHalf::LOW => {
                                                    let high = *r & 0xFF00;
                                                    *r = high & (b as u16);
                                                },
                                                RegisterHalf::HIGH => {
                                                    let low = *r & 0x00FF;
                                                    *r = low & ((b as u16) << 8);
                                                },
                                                _ => unreachable!(),
                                            };
                                        });
                                    },
                                    Operand::Memory(m) => {
                                        self.ram[m.0 as usize] = b;
                                    },
                                };
                            },
                            Value::Word(w) => {
                                match operand {
                                    Operand::Register(r) => {
                                        self.cpu.mutate_register(&r, |r: &mut u16, _h: RegisterHalf| {
                                            *r = w;
                                        });
                                    },
                                    Operand::Memory(m) => {
                                        self.write_word(&m, w);
                                    },
                                };
                            },
                        };
                        self.update_flags("CZSOPA", Some(operand_value), Some(sum), Some(true));
                    },
                    _ => (),
                }
            },
            _ => (),
        }
        match self.ram[address.0 as usize] >> 1 {
            0b1111111 => {
                instruction_size += 1;
                let operand_size: OperandSize = (self.ram[address.0 as usize] & 0b00000001).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                instruction_size += Self::get_instruction_size_extension_by_mod(&modrm.0);
                let (before, after) = match modrm.1 {
                    // INC, modr/m
                    0b00000000 => {
                        match modrm.0 {
                            ModRMMod::Register => {
                                match modrm.2 {
                                    RM::Register(e) => {
                                        let before = self.cpu.get_register(&e);
                                        self.inc_register(&e);
                                        let after = self.cpu.get_register(&e);
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
                        }
                    },
                    _ => unreachable!(),
                };
                self.update_flags("ZSOPA", Some(before), Some(after), Some(true))
            },
            0b01100011 => {
                instruction_size += 1;
                let operand_size: OperandSize = self.ram[address.0 as usize].bit(0).try_into().unwrap();
                let modrm = Self::parse_modrm(&operand_size, &self.ram[address.0 as usize + 1]);
                match modrm.1 {
                    // MOV, mod/rm, immediate
                    0b00000000 => {
                        let operand = match modrm.0 {
                            ModRMMod::Register => {
                                match modrm.2 {
                                    RM::Register(e) => Operand::Register(e),
                                    _ => unreachable!(),
                                }
                            },
                            _ => {
                                Operand::Memory(self.calculate_address_by_modrm(&address, modrm.0, modrm.2, &segment_override))
                            },
                        };
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
                        match operand {
                            Operand::Register(r) => {
                                self.cpu.mutate_register(&r, |r: &mut u16, h: RegisterHalf| {
                                    match h {
                                        RegisterHalf::FULL => *r = match immediate {
                                            Value::Word(w) => w,
                                            _ => unreachable!(),
                                        },
                                        RegisterHalf::HIGH => *r = match immediate {
                                            Value::Byte(b) => {
                                                let low = *r & 0x00FF;
                                                low & ((b as u16) << 8)
                                            },
                                            _ => unreachable!(),
                                        },
                                        RegisterHalf::LOW => *r = match immediate {
                                            Value::Byte(b) => {
                                                let high = *r & 0xFF00;
                                                high & (b as u16)
                                            },
                                            _ => unreachable!(),
                                        },
                                    }
                                });
                            },
                            Operand::Memory(m) => {
                                match immediate {
                                    Value::Byte(b) => {
                                        self.ram[m.0 as usize] = b
                                    },
                                    Value::Word(w) => {
                                        self.write_word(&m, w);
                                    },
                                };
                            },
                        };
                    },
                    _ => unreachable!(),
                };
            },
            _ => (),
        }
        self.cpu.ip += instruction_size;
        Ok(())
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
        };
        let msb_after = match after {
            Value::Byte(b) => b >> 7,
            Value::Word(w) => (w >> 15) as u8,
        };
        if msb_before != msb_after {
            self.cpu.flags |= Flags::CF.bits();
        } else {
            self.cpu.flags |= !Flags::CF.bits();
        }
    }

    fn update_parity_flag(&mut self, after: &Value) {
        let lsb = match after {
            Value::Byte(b) => *b,
            Value::Word(w) => *w as u8,
        };
        let parity = lsb.count_ones() % 2 == 0;
        if parity  {
            self.cpu.flags |= Flags::PF.bits();
        } else {
            self.cpu.flags |= !Flags::PF.bits();
        }
    }

    fn update_aux_carry_flag(&mut self, before: &Value, after: &Value) {
        let third_bit_before = match before {
            Value::Byte(b) => (b >> 3) & 0b00000001,
            Value::Word(w) => ((w >> 3) as u8) & 0b00000001,
        };
        let third_bit_after = match after {
            Value::Byte(b) => (b >> 3) & 0b00000001,
            Value::Word(w) => ((w >> 3) as u8) & 0b00000001,
        };
        if third_bit_before != third_bit_after {
            self.cpu.flags |= Flags::AF.bits();
        } else {
            self.cpu.flags |= !Flags::AF.bits();
        }
    }

    fn update_zero_flag(&mut self, after: &Value) {
        let is_zero = match after {
            Value::Byte(b) => *b == 0,
            Value::Word(w) => *w == 0,
        };
        if is_zero {
            self.cpu.flags |= Flags::ZF.bits();
        } else {
            self.cpu.flags |= !Flags::ZF.bits();
        }
    }

    fn update_sign_flag(&mut self, after: &Value) {
        let sign = match after {
            Value::Byte(b) => (b >> 7) == 1,
            Value::Word(w) => ((w >> 15) as u8) == 1,
        };
        if sign {
            self.cpu.flags |= Flags::SF.bits();
        } else {
            self.cpu.flags |= !Flags::SF.bits();
        }
    }

    fn update_overflow_flag(&mut self, before: &Value, after: &Value, increasing: &bool) {
        let value_before = match before {
            Value::Byte(b) => *b as u16,
            Value::Word(w) => *w,
        };
        let value_after = match after {
            Value::Byte(b) => *b as u16,
            Value::Word(w) => *w,
        };
        // TODO: What if overflows and becomes the same value?
        let overflow = (*increasing && value_before > value_after) || (!*increasing && value_before < value_after);
        if overflow {
            self.cpu.flags |= Flags::OF.bits();
        } else {
            self.cpu.flags |= !Flags::OF.bits();
        }
    }

    fn read_word(&self, m: &U20) -> u16 {
        self.ram[m.0 as usize] as u16 | ((self.ram[(m.0 + 1) as usize] as u16) << 8)
    }

    fn write_word(&mut self, m: &U20, w: u16) {
        self.ram[m.0 as usize] = w as u8;
        self.ram[(m.0 + 1) as usize] = (w >> 8) as u8
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
}

struct U20(u32);

impl U20 {
    fn new(segment: u16, offset: u16) -> Self {
        U20(u32::from(segment) * 16 + u32::from(offset))
    }
}

fn main() {
    panic!("Nothing here yet");
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
        let run = emulator.run();

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
        let run = emulator.run();

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
        let run = emulator.run();

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
        let run = emulator.run();

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
        let run = emulator.run();

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
        let run = emulator.run();

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
        let run = emulator.run();

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
        let run = emulator.run();

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
        let run = emulator.run();

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
        let run = emulator.run();

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
        let run = emulator.run();

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
        let run = emulator.run();

        match run {
            Ok(()) => (),
            Err(_error) => {
                assert_eq!(emulator.cpu.cx, 0xAAAA);
            }
        }
    }
}
