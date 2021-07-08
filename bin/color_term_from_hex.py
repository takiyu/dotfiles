#!/usr/bin/python3

def rgb_to_xterm(r, g, b):
    N = []
    for i, n in enumerate([47, 68, 40, 40, 40, 21]):
        N.extend([i]*n)

    mx = max(r, g, b)
    mn = min(r, g, b)

    if (mx - mn) * (mx + mn) <= 6250:
        c = 24 - (252 - ((r + g + b) // 3)) // 10
        if 0 <= c <= 23:
            return 232 + c

    return 16 + 36 * N[r] + 6 * N[g] + N[b]

print('Convert RGB to XTerm Color')
r = int(input('R: 0x'), base=16)
g = int(input('G: 0x'), base=16)
b = int(input('B: 0x'), base=16)

col = rgb_to_xterm(r, g, b)
print('color: ' + str(col))
