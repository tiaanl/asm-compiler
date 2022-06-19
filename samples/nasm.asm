main:
    stc
    clc

    mov bx, data
    mov [bx + 1], 0x10

    add ax, (10 + 4) * 3
    add al, 10
    jmp main

data:
    db 0, 0, 0, 0, 0
