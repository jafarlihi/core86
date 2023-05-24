import dearpygui.dearpygui as dpg
import socket
import json


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
        self.mem = self.client.get_memory(self.addr, 100)


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

    def update(self):
        dpg.set_value(self.registers_text, "AX: " + str(self.state.cpu['ax'])
                      + "\nBX: " + str(self.state.cpu['bx'])
                      + "\nCX: " + str(self.state.cpu['cx']))


if __name__ == '__main__':
    dpg.create_context()
    dpg.create_viewport()
    dpg.setup_dearpygui()

    state = State()
    state.update()

    registers_text_element = ""
    disasm_text_element = ""

    with dpg.window(label="Disassembly"):
        disasm_text_element = dpg.add_text("")

    with dpg.window(label="Registers", pos=[0, 400], width=400, height=100):
        registers_text_element = dpg.add_input_text(multiline=True, enabled=False, width=350, height=65)

    gui = GUI(state, registers_text_element, disasm_text_element)

    with dpg.window(label="Actions", pos=[400, 400]):
        dpg.add_button(label="Step", callback=gui.step)

    dpg.show_viewport()
    dpg.start_dearpygui()
    dpg.destroy_context()
