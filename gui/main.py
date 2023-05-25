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

    def __init__(self, state, disasm_text, registers_text, flags_text):
        self.state = state
        self.disasm_text = disasm_text
        self.registers_text = registers_text
        self.flags_text = flags_text

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
        dpg.set_value(self.flags_text,
                      "CF: " + str(self.state.cpu['flags'] & 1)
                      + " PF: " + str(self.state.cpu['flags'] >> 2 & 1)
                      + " AF: " + str(self.state.cpu['flags'] >> 4 & 1)
                      + " ZF: " + str(self.state.cpu['flags'] >> 6 & 1)
                      + "\nSF: " + str(self.state.cpu['flags'] >> 7 & 1)
                      + " TF: " + str(self.state.cpu['flags'] >> 8 & 1)
                      + " IF: " + str(self.state.cpu['flags'] >> 9 & 1)
                      + " DF: " + str(self.state.cpu['flags'] >> 10 & 1)
                      + "\nOF: " + str(self.state.cpu['flags'] >> 11 & 1))
        dpg.set_value(self.disasm_text, disassemble(self.state.mem))


if __name__ == '__main__':
    dpg.create_context()
    dpg.create_viewport()
    dpg.setup_dearpygui()

    state = State()
    state.update()

    disasm_text_element = ""
    registers_text_element = ""
    flags_text_element = ""

    with dpg.window(label="Disassembly", height=400, width=525):
        disasm_text_element = dpg.add_text("")

    with dpg.window(label="Registers", pos=[0, 400], width=400, height=100):
        registers_text_element = dpg.add_input_text(multiline=True, enabled=False, width=350, height=65)

    with dpg.window(label="Flags", pos=[0, 500], width=400, height=100):
        flags_text_element = dpg.add_input_text(multiline=True, enabled=False, width=350, height=65)

    gui = GUI(state, disasm_text_element, registers_text_element, flags_text_element)

    with dpg.window(label="Actions", pos=[400, 400], width=125):
        dpg.add_button(label="Step", callback=gui.step, width=100)
        dpg.add_button(label="Run", callback=gui.run, width=100)

    dpg.show_viewport()
    dpg.start_dearpygui()
    dpg.destroy_context()
