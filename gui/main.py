import dearpygui.dearpygui as dpg
from iced_x86 import *
import socket
import json


def padded_hex(value):
    padding = 6
    return f"{value:#0{padding}x}"


def disassemble(raw):
    decoder = Decoder(16, raw, ip=0)
    formatter = Formatter(FormatterSyntax.NASM)
    result = ""
    for insn in decoder:
        disasm = formatter.format(insn)
        result += disasm + "\n"
    return result


class Client:
    HOST = "127.0.0.1"
    PORT = 3333

    def get_memory(self, address, size):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.connect((self.HOST, self.PORT))
            s.sendall(bytes("mem;" + str(address) + ";" + str(size) + "\n", 'ascii'))
            data = s.recv(size)
            return data

    def get_cpu(self):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.connect((self.HOST, self.PORT))
            s.sendall(bytes("cpu\n", 'ascii'))
            data = s.recv(1024)
            return data

    def step(self):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.connect((self.HOST, self.PORT))
            s.sendall(bytes("step\n", 'ascii'))


class State:
    client = Client()

    def update(self):
        self.cpu = json.loads(self.client.get_cpu().decode('ascii'))
        self.addr = self.cpu['cs'] * 16 + self.cpu['ip']
        self.mem = self.client.get_memory(self.addr, 1024)


class GUI:
    client = Client()

    def __init__(self, state, registers_text, disasm_text):
        self.state = state
        self.registers_text = registers_text
        self.disasm_text = disasm_text

    def step(self):
        self.client.step()
        self.state.update()
        self.update()

    def run(self):
        pass

    def update(self):
        dpg.set_value(self.registers_text,
                      "AX: " + padded_hex(self.state.cpu['ax'])
                      + " SI: " + padded_hex(self.state.cpu['si'])
                      + " CS: " + padded_hex(self.state.cpu['cs'])
                      + " IP: " + padded_hex(self.state.cpu['ip'])
                      + "\nBX: " + padded_hex(self.state.cpu['bx'])
                      + " DI: " + padded_hex(self.state.cpu['di'])
                      + " DS: " + padded_hex(self.state.cpu['ds'])
                      + " FL: " + padded_hex(self.state.cpu['flags'])
                      + "\nCX: " + padded_hex(self.state.cpu['cx'])
                      + " SP: " + padded_hex(self.state.cpu['sp'])
                      + " ES: " + padded_hex(self.state.cpu['es'])
                      + "\nDX: " + padded_hex(self.state.cpu['dx'])
                      + " BP: " + padded_hex(self.state.cpu['bp'])
                      + " SS: " + padded_hex(self.state.cpu['ss']))
        dpg.set_value(self.disasm_text, disassemble(self.state.mem))


if __name__ == '__main__':
    dpg.create_context()
    dpg.create_viewport()
    dpg.setup_dearpygui()

    state = State()
    state.update()

    registers_text_element = ""
    disasm_text_element = ""

    with dpg.window(label="Disassembly", height=400, width=525):
        disasm_text_element = dpg.add_text("")

    with dpg.window(label="Registers", pos=[0, 400], width=400, height=100):
        registers_text_element = dpg.add_input_text(multiline=True, enabled=False, width=350, height=65)

    gui = GUI(state, registers_text_element, disasm_text_element)

    with dpg.window(label="Actions", pos=[400, 400], width=125):
        dpg.add_button(label="Step", callback=gui.step, width=100)
        dpg.add_button(label="Run", callback=gui.run, width=100)

    dpg.show_viewport()
    dpg.start_dearpygui()
    dpg.destroy_context()
