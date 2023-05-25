mod emulator;
mod bios;
mod video;

use std::net::{TcpListener, TcpStream};
use std::io::{Write, BufReader, BufRead};
use std::sync::{Arc, Mutex};
use ncurses::*;
use emulator::{Emulator, U20};
use video::update_mda_screen;
use bios::{init_rom_configuration, init_ivt, load_bootsector};

fn read_file(file: &str) -> std::io::Result<Vec<u8>> {
    use std::fs::File;
    use std::io::Read;

    let mut file = File::open(file).unwrap();

    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();

    return Ok(data);
}

fn step(emulator: &mut Emulator) -> Result<(), Box<dyn std::error::Error>> {
    emulator.service_interrupts();
    update_mda_screen(&emulator);
    match emulator.execute() {
        Ok(()) => Ok(()),
        Err(error) => return Err(error),
    }
}

fn handle_client(mut stream: TcpStream, emulator: Arc<Mutex<Emulator>>) {
    let mut reader = BufReader::new(stream.try_clone().unwrap());
    let mut request = String::new();
    let _ = reader.read_line(&mut request);
    if request.to_lowercase().starts_with("cpu") {
        stream.write(format!("{}", serde_json::to_string(&emulator.lock().unwrap().cpu).unwrap()).as_bytes()).unwrap();
    } else if request.to_lowercase().starts_with("step") {
        step(&mut emulator.lock().unwrap()).unwrap();
    } else if request.to_lowercase().starts_with("mem") {
        let command_parts = request.split(";");
        let command: Vec<&str> = command_parts.collect();
        let address = command[1].parse::<usize>().unwrap();
        let size = command[2].trim().parse::<usize>().unwrap();
        let result = &emulator.lock().unwrap().ram[address..address + size];
        stream.write(result).unwrap();
    }
}

fn main() {
    use iced_x86::{Decoder, DecoderOptions};
    use std::thread;

    let disk = read_file("../Disk01.img").unwrap().into_boxed_slice();
    //disk[0x77] = 0x73; // Hack for DOS 2.0
    let emulator = Emulator::new(disk);
    let emulator = Arc::new(Mutex::new(emulator));
    {
        let emulator = Arc::clone(&emulator);
        thread::spawn(move || {
            let listener = TcpListener::bind("0.0.0.0:3333").unwrap();
            for stream in listener.incoming() {
                match stream {
                    Ok(stream) => {
                        let emulator = Arc::clone(&emulator);
                        thread::spawn(move|| {
                            handle_client(stream, emulator)
                        });
                    }
                    Err(_e) => {
                    }
                }
            }
        });
    }
    {
        let mut emulator_unlocked = emulator.lock().unwrap();
        init_rom_configuration(&mut emulator_unlocked);
        init_ivt(&mut emulator_unlocked);
        load_bootsector(&mut emulator_unlocked).unwrap();
    }
    initscr();
    loop {
        getch();
        let mut emulator_unlocked = emulator.lock().unwrap();
        let address = U20::new(emulator_unlocked.cpu.cs, emulator_unlocked.cpu.ip);
        let bytes = &emulator_unlocked.ram[address.0 as usize..address.0 as usize + 20];
        let mut decoder = Decoder::with_ip(16, bytes, 0, DecoderOptions::NONE);
        let instruction = decoder.decode();
        mvprintw(26, 0, &format!("{} {}                        ", emulator_unlocked.cpu.cs, emulator_unlocked.cpu.ip));
        mvprintw(27, 0, &[instruction.to_string(), "                        ".to_string()].join(""));
        let instruction = decoder.decode();
        mvprintw(28, 0, &[instruction.to_string(), "                        ".to_string()].join(""));
        let instruction = decoder.decode();
        mvprintw(29, 0, &[instruction.to_string(), "                        ".to_string()].join(""));
        let instruction = decoder.decode();
        mvprintw(30, 0, &[instruction.to_string(), "                        ".to_string()].join(""));
        step(&mut emulator_unlocked).unwrap();
    }
}
