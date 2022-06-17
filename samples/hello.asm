mov     ax, cs
mov     ds, ax              ; move cs into ds
mov     dx, [hello_world]   ; move dx to the offset of the message
mov     ah, 9h              ; set the print command
int     21h                 ; invoke dos interrupt
mov     ax, 4c00h           ; set the terminate command with code 0
int     21h                 ; invode dos interrupt

hello_world
        db 'Hello, World!$'
