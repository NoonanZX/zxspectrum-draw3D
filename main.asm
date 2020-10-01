                    DEVICE ZXSPECTRUM48

                    ORG #8000


;                    DEFINE TEST

                    INCLUDE "macros.inc"

                    IFDEF TEST
screen              EQU #4000
                    ELSE
screen              EQU #C000
                    ENDIF


code                DI

                    LD A,2
                    OUT (254),A

                    LD B,24
                    LD D,0
                    LD HL,#4000
                    CALL mem.fill

                    LD B,3
                    LD D,64+4
                    CALL mem.fill

.x_min              EQU 50
.x_max              EQU 150
.y_min              EQU 50
.y_max              EQU 150

                    LD B,.x_min
                    LD C,.y_min
                    LD D,.x_max
                    LD E,.y_max
                    ;CALL draw2DEX.set_viewport

                    ;CALL draw_axis
                    ;CALL draw_viewport

                    LD BC,256
.loop               PUSH BC

                    IFNDEF TEST
                    LD B,24
                    LD D,0
                    LD HL,screen
                    CALL mem.fill
                    ENDIF

                    LD BC,#0800
                    LD HL,.vertices
                    CALL draw3D.set_vertices

                    LD HL,#0001
                    CALL draw3D.draw_line
                    LD HL,#0203
                    CALL draw3D.draw_line
                    LD HL,#0405
                    CALL draw3D.draw_line
                    LD HL,#0607
                    CALL draw3D.draw_line

                    LD HL,#0002
                    CALL draw3D.draw_line
                    LD HL,#0103
                    CALL draw3D.draw_line
                    LD HL,#0406
                    CALL draw3D.draw_line
                    LD HL,#0507
                    CALL draw3D.draw_line

                    LD HL,#0004
                    CALL draw3D.draw_line
                    LD HL,#0105
                    CALL draw3D.draw_line
                    LD HL,#0206
                    CALL draw3D.draw_line
                    LD HL,#0307
                    CALL draw3D.draw_line

                    LD B,24
                    LD HL,screen
                    LD DE,#4000
                    CALL mem.copy

                    LD A,(draw3D.yaw)
                    INC A
                    LD (draw3D.yaw),A
                    LD A,(draw3D.roll)
                    INC A
                    LD (draw3D.roll),A
                    LD A,(draw3D.pitch)
                    INC A
                    LD (draw3D.pitch),A

                    POP BC
                    DEC BC
                    LD A,B
                    OR C
;                    JP NZ,.loop
                    JP .loop

                    LD A,1
                    OUT (254),A
                    HALT

.size_x             EQU 100
.size_y             EQU 100
.size_z             EQU 100

.vertices           ;BYTE -75, 0, 0

                    BYTE -.size_x/2, -.size_y/2, -.size_z/2
                    BYTE +.size_x/2, -.size_y/2, -.size_z/2
                    BYTE -.size_x/2, +.size_y/2, -.size_z/2
                    BYTE +.size_x/2, +.size_y/2, -.size_z/2
                    BYTE -.size_x/2, -.size_y/2, +.size_z/2
                    BYTE +.size_x/2, -.size_y/2, +.size_z/2
                    BYTE -.size_x/2, +.size_y/2, +.size_z/2
                    BYTE +.size_x/2, +.size_y/2, +.size_z/2


draw_axis           LD DE,#0060
                    LD HL,#FF60
                    CALL draw2D.draw_horizontal_line
                    LD DE,#8000
                    LD HL,#80C0
                    JP draw2D.draw_vertical_line

                    
draw_viewport       LD A,(draw2DEX.x_min)
                    LD D,A
                    LD A,(draw2DEX.y_min)
                    INC A
                    LD E,A
                    LD A,(draw2DEX.x_max)
                    LD H,A
                    CALL draw2D.draw_horizontal_line

                    LD A,(draw2DEX.x_min)
                    LD D,A
                    LD A,(draw2DEX.y_max)
                    DEC A
                    LD E,A
                    LD A,(draw2DEX.x_max)
                    LD H,A
                    CALL draw2D.draw_horizontal_line

                    LD A,(draw2DEX.x_min)
                    INC A
                    LD D,A
                    LD A,(draw2DEX.y_min)
                    LD E,A
                    LD A,(draw2DEX.y_max)
                    LD L,A
                    CALL draw2D.draw_vertical_line

                    LD A,(draw2DEX.x_max)
                    DEC A
                    LD D,A
                    LD A,(draw2DEX.y_min)
                    LD E,A
                    LD A,(draw2DEX.y_max)
                    LD L,A
                    JP draw2D.draw_vertical_line


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


;screen_buffer EQU #C000 ; hack
/*
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
/*
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
