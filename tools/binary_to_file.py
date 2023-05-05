#!/usr/bin/env python3

import sys

def bitstring_to_bytes(s):
    v = int(s, 2)
    b = bytearray()
    while v:
        b.append(v & 0xff)
        v >>= 8
    return bytes(b[::-1])

if __name__ == '__main__':
    binary = bitstring_to_bytes(sys.argv[1])
    ba = bytearray(binary)
    file = open("file.bin", "wb")
    for byte in ba:
        file.write(byte.to_bytes(1, byteorder='little'))
