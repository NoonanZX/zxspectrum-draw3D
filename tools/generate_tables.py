import math
import random


def byte_unsigned_to_signed(x):
    return x if x < 128 else x - 256

def byte_signed_to_unsigned(x):
    return x if x >= 0 else x + 256

def word_unsigned_to_signed(x):
    return x if x < 32768 else x - 65536

def word_signed_to_unsigned(x):
    return x if x >= 0 else x + 65536

def byte_to_float(x):
    return (x+128)/255*2-1

def float_to_byte(x):
    if x >= 0:
        return int(x * 127+0.5)
    else:
        return -int(-x * 127+0.5)


def split_into_blocks(data, block_size):
    if block_size is not None:
        return [data[i:i+block_size] for i in range(0, len(data), block_size)]
    else:
        return [data]


def generate_byte_table(data, name = None, align = None, width = 16, paragraph_height = None, prefix = '', suffix = ''):
    r = ''

    if align is not None:
        r += f"\tALIGN {align}\n"

    if name is not None:
        r += f"{name}:\n"
    elif align is not None:
        r += "\n"

    data = split_into_blocks(data, width)
    data = split_into_blocks(data, paragraph_height)

    r += "\n".join([''.join([f"\tBYTE {','.join([prefix + '#{:02X}'.format(byte_signed_to_unsigned(x)) + suffix for x in line])}\n" for line in paragraph]) for paragraph in data])

    return r


def generate_word_table(data, name = None, align = None, width = 16, paragraph_height = None, hi_prefix = '', hi_suffix = '', lo_prefix = '', lo_suffix = ''):
    r = ''

    hi = [word_signed_to_unsigned(x) // 256 for x in data]
    lo = [word_signed_to_unsigned(x) % 256 for x in data]

    r += generate_byte_table(lo, name, align, width, paragraph_height, lo_prefix, lo_suffix)
    r += "\n"

    if align is not None:
        if len(data) % align == 0:
            align = None
            
    r += generate_byte_table(hi, None, align, width, paragraph_height, hi_prefix, hi_suffix)

    return r


def screen_line_addr(i):
    j = 191 - i
    k = j // 64
    j = j % 64
    return (k * 64 + j % 8 * 8 + j // 8) * 32


with open("screen_table.dat", "w") as f:
    table = [screen_line_addr(i) for i in range(192)]
    f.write(generate_word_table(table, name="screen_table", align = 256, width = 8, paragraph_height = 8, hi_prefix = 'HIGH(screen)+'))

with open("muldiv.dat", "w") as f:
    pow2s_table = [byte_unsigned_to_signed(i)**2 for i in range(256)]
    pow2u_table = [i**2 for i in range(256)]
    f.write(generate_word_table(pow2s_table, name = "pow2s_table", align = 256))
    f.write("\n")
    f.write(generate_word_table(pow2u_table, name = "pow2u_table"))

with open("sincos.dat", "w") as f:
    sin_table = [int(math.sin(i/128*math.pi) * 255) for i in range(128)]
    f.write(generate_byte_table(sin_table, name = "sin_table", align = 256))
