use crate::emulator::{Emulator, U20, RegisterEncoding, RegisterEncoding8, RegisterEncoding16, Flags, Value};

pub const BIOS_SEG: u16 = 0xF000;
pub const ROM_CONFIGURATION_ADDR: u16 = 0xE6F5;
pub const EQUIPMENT_SEG: u16 = 0x0040;
pub const EQUIPMENT_ADDR: u16 = 0x0010;

pub fn init_rom_configuration(emulator: &mut Emulator) {
    emulator.write_word(&U20::new(BIOS_SEG, ROM_CONFIGURATION_ADDR), 8);
    emulator.write_mem(&U20::new(BIOS_SEG, ROM_CONFIGURATION_ADDR + 2), &Value::Byte(0xFC));
    emulator.write_word(&U20::new(EQUIPMENT_SEG, EQUIPMENT_ADDR), 0b0000000000110001);
}

pub fn init_ivt(emulator: &mut Emulator) {
    const IRET: u8 = 0b11001111;
    for int in 0..0xFF {
        emulator.write_word(&U20::new(0, int * 4), int);
        emulator.write_word(&U20::new(0, int * 4 + 2), BIOS_SEG);
        emulator.write_mem(&U20::new(BIOS_SEG, int), &Value::Byte(IRET));
    }
}

pub fn load_bootsector(emulator: &mut Emulator) -> Result<(), Box<dyn std::error::Error>> {
    if emulator.disk[510] != 0x55 && emulator.disk[511] != 0xAA {
        return Err("Disk not bootable".into());
    }
    emulator.ram[0x7c00..0x7c00 + 512].copy_from_slice(&emulator.disk[..512]);
    emulator.cpu.cs = 0x0000;
    emulator.cpu.ip = 0x7c00;
    Ok(())
}

pub fn execute_isr(emulator: &mut Emulator, int: u8) {
    match int {
        // Equipment fetch
        0x11 => {
            emulator.cpu.ax = emulator.read_word(&U20::new(EQUIPMENT_SEG, EQUIPMENT_ADDR));
        },
        // Disk services
        0x13 => {
            let disk = emulator.cpu.read_register8(&RegisterEncoding8::DL);
            let argument = emulator.cpu.read_register8(&RegisterEncoding8::AH);
            if disk != 0 {
                match argument {
                    0x15 => {
                        emulator.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH), &Value::Byte(0x0));
                    },
                    _ => emulator.cpu.flags |= Flags::CF.bits(),
                };
                return;
            }
            match argument {
                0x0 => {
                    // Success code == 0x0
                    emulator.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH), &Value::Byte(0x0));
                    // CF = !Success
                    emulator.cpu.flags &= !Flags::CF.bits();
                },
                0x2 => {
                    let sectors = emulator.cpu.read_register8(&RegisterEncoding8::AL);
                    let head = emulator.cpu.read_register8(&RegisterEncoding8::DH);
                    let cx = emulator.cpu.read_register16(&RegisterEncoding16::CX);
                    let address = U20::new(emulator.cpu.read_register16(&RegisterEncoding16::ES), emulator.cpu.read_register16(&RegisterEncoding16::BX));
                    let cylinder = ((cx & 0xFf00) >> 8) + ((cx & 0x00C0) << 2);
                    let sector = (cx & 0x003F) as u8;
                    let (success, bytes_read) = read_chs(emulator, address, cylinder, head, sector, sectors);
                    emulator.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AL), &Value::Byte(bytes_read));
                    if success {
                        emulator.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH), &Value::Byte(0x0));
                        emulator.cpu.flags &= !Flags::CF.bits();
                    } else {
                        // 0x1 is INVALID_PARAM, 0x4 is SECTOR_NOT_FOUND
                        emulator.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH), &Value::Byte(0x1));
                        emulator.cpu.flags |= Flags::CF.bits();
                    }
                },
                _ => unimplemented!(),
            };
        },
        // Disk change
        0x16 => {
            emulator.cpu.write_register(&RegisterEncoding::RegisterEncoding8(RegisterEncoding8::AH), &Value::Byte(0x0));
            emulator.cpu.flags &= !Flags::CF.bits();
        },
        _ => unimplemented!(),
    };
}

fn read_chs(emulator: &mut Emulator, address: U20, cylinder: u16, head: u8, sector: u8, sectors: u8) -> (bool, u8) {
    // TODO: Return error if attempt was made to read outside of {1, 40, 9} (H,T,S) 180k floppy
    if sectors == 0 {
        return (false, 0);
    }
    let sector_size = 512;
    let lba = cylinder as u32 * 1 + head as u32 * 9 + (sector - 1) as u32;
    return (true, read_lba(emulator, address, lba * sector_size, sectors as u32 * sector_size));
}

fn read_lba(emulator: &mut Emulator, address: U20, lba: u32, size: u32) -> u8 {
    emulator.ram[address.0 as usize..address.0 as usize + size as usize].copy_from_slice(&emulator.disk[lba as usize..lba as usize + size as usize]);
    return size as u8;
}
