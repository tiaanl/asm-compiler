main:
    ; constants
    another_label value_1 equ 10
    value_2 equ 20

    stc
    clc

    mov bx, data
    mov byte [bx + 1], value_1

    add ax, (10 + 4) * 3 + value_2
    add al, 10
    jmp main

data:
    db 0, 0, 0, 0, 0
