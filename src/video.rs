use ncurses::*;
use codepage_437::{CP437_CONTROL, FromCp437};
use crate::emulator::{Emulator, U20};

pub fn update_mda_screen(emulator: &Emulator) {
    let address = U20::new(0xb000, 0x0000);
    let vector: Vec<u8> =
        emulator.ram[address.0 as usize..address.0 as usize + 25 * 80 * 2]
        .iter()
        .enumerate()
        .filter(|(i, _)| i % 2 == 0)
        .map(|(_, byte)| *byte)
        .collect();
    let unicode = String::from_cp437(vector, &CP437_CONTROL);
    let unicode = unicode.replace('\0', " ");
    let mut line = 0;
    let mut column = 0;
    for u in unicode.into_bytes() {
        mvaddch(line, column, u as u32);
        column += 1;
        if column == 80 {
            column = 0;
            line += 1;
        }
    }
    refresh();
}
