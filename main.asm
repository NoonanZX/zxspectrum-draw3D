                    DEVICE ZXSPECTRUM128

                    ORG #8000


;                    DEFINE TEST

                    INCLUDE "macros.inc"

                    IFDEF TEST
screen              EQU #4000
                    ELSE
screen              EQU #C000
                    ENDIF


code                DI
                    LD SP,#C000

.memory_state       EQU 23388

                    IFNDEF TEST
                    LD A,%00010111
                    LD (.memory_state),A
                    LD BC,#7FFD
                    OUT (C),A
                    ENDIF

                    LD B,3
                    LD D,64+4
                    LD HL,#5800
                    CALL mem.fill
                    LD B,3
                    LD HL,#D800
                    CALL mem.fill

                    XOR A
;                    LD A,2
                    OUT (254),A

                    LD BC,256
.loop               PUSH BC

                    IFNDEF TEST
                    LD B,24
                    LD D,0
                    LD HL,screen
                    CALL mem.fill
                    ENDIF

                    CALL draw

                    IFNDEF TEST
                    EI:HALT:DI
                    LD A,(.memory_state)
                    XOR %00001010
                    LD (.memory_state),A
                    LD BC,#7FFD
                    OUT (C),A
                    ENDIF

                    POP BC
                    DEC BC
                    LD A,B
                    OR C
;                    JP NZ,.loop
                    JP .loop

                    LD A,1
                    OUT (254),A

                    HALT


                    INCLUDE "demo.asm"
                    INCLUDE "mem.asm"
                    INCLUDE "muldiv.asm"
                    INCLUDE "sincos.asm"
                    INCLUDE "misc.asm"
                    INCLUDE "draw2D.asm"
                    INCLUDE "draw2DEX.asm"
                    INCLUDE "draw3D.asm"
code_size           EQU $-code

                    ALIGN 256
data                INCLUDE "screen_table.dat"
                    INCLUDE "muldiv.dat"
                    INCLUDE "sincos.dat"
;                    INCLUDE "draw2D.dat"
                    INCLUDE "draw3D.dat"
data_size           EQU $-data

                    DISPLAY "Size: ",/D,$-code
                    DISPLAY "Code: ",/D,code_size
                    DISPLAY "Data: ",/D,data_size

                    SAVESNA "c:/tools/unreal/qsave1.sna", code
                    LABELSLIST "c:/tools/unreal/user.l"


/* TODO
;    LD A,2
;    OUT (254),A

    LD BC,100
1:
    PUSH BC

    LD B,24
    LD D,0
    LD HL,screen_buffer
    CALL mem.fill

    LD HL,.model
    CALL draw3D.model

    LD B,24
    LD HL,screen_buffer
    LD DE,#4000
    CALL mem.copy

    LD A,(draw3D.yaw)
    INC A
    LD (draw3D.yaw),A
    RRA
    JR NC,0F
    LD A,(draw3D.roll)
    INC A
    LD (draw3D.roll),A
    RRA
    JR NC,0F
    LD A,(draw3D.pitch)
    INC A
    LD (draw3D.pitch),A
0:

    POP BC
    DEC BC
    LD A,B
    OR C
;    JP NZ,1B
    JP 1B

    LD A,1
    OUT (254),A
*/
/* TODO
.model:
    box 100,100,100,.solid,.solid,.checkerboard_2x2,.rabica,.checkerboard,.checkerboard
;    box 100,100,100,#3D88,#3D90,#3D98,#3DA0,#3DA8,#3DB0

    ALIGN 8
.solid
    BYTE %11111111
    BYTE %11111111
    BYTE %11111111
    BYTE %11111111
    BYTE %11111111
    BYTE %11111111
    BYTE %11111111
    BYTE %11111111
.checkerboard:
    BYTE %10101010
    BYTE %01010101
    BYTE %10101010
    BYTE %01010101
    BYTE %10101010
    BYTE %01010101
    BYTE %10101010
    BYTE %01010101
.checkerboard_2x2:
    BYTE %11001100
    BYTE %11001100
    BYTE %00110011
    BYTE %00110011
    BYTE %11001100
    BYTE %11001100
    BYTE %00110011
    BYTE %00110011
.zigzag
    BYTE %00010100
    BYTE %00100010
    BYTE %01000001
    BYTE %10001000
    BYTE %00010100
    BYTE %00100010
    BYTE %01000001
    BYTE %10001000
.rabica
    BYTE %00011000
    BYTE %00100100
    BYTE %01000010
    BYTE %10000001
    BYTE %10000001
    BYTE %01000010
    BYTE %00100100
    BYTE %00011000
.smile:
    BYTE %00000000
    BYTE %00000000
    BYTE %00101000
    BYTE %00000000
    BYTE %00010000
    BYTE %01000100
    BYTE %00111000
    BYTE %00000000
*/
