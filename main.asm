                    DEVICE ZXSPECTRUM48

                    ORG #8000

code
                    INCLUDE "macros.inc"
;                    INCLUDE "models.inc"

                    DI

                    XOR A
;                    LD A,2
                    OUT (254),A

                    LD B,24
                    LD D,0
                    LD HL,#4000
                    CALL mem.fill

                    LD B,3
                    LD D,64+5
                    CALL mem.fill

                    MACRO test_line dx, dy
                        LD DE,100+100*256
                        LD HL,(100+dx)+(100+dy)*256
                        CALL draw2D.draw_line
                    ENDM

                    test_line -50,-50
                    test_line -50,-25
                    test_line -50,0
                    test_line -50,+25
                    test_line -50,+50
                    test_line -25,-50
                    test_line -25,-25
                    test_line -25,0
                    test_line -25,+25
                    test_line -25,+50
                    test_line 0,-50
                    test_line 0,-25
                    test_line 0,0
                    test_line 0,+25
                    test_line 0,+50
                    test_line +25,-50
                    test_line +25,-25
                    test_line +25,0
                    test_line +25,+25
                    test_line +25,+50
                    test_line +50,-50
                    test_line +50,-25
                    test_line +50,0
                    test_line +50,+25
                    test_line +50,+50

                    MACRO test_horz_line x1, x2, y
                        LD D,x1
                        LD E,y
                        LD H,x2
                        CALL draw2D.draw_horizontal_line
                    ENDM

                    test_horz_line 225-8, 225+8, 155-8
                    test_horz_line 225-6, 225+6, 155-6
                    test_horz_line 225-4, 225+4, 155-4
                    test_horz_line 225-2, 225+2, 155-2
                    test_horz_line 225, 225, 155
                    test_horz_line 225+2, 225-2, 155+2
                    test_horz_line 225+4, 225-4, 155+4
                    test_horz_line 225+6, 225-6, 155+6
                    test_horz_line 225+8, 225-8, 155+8

                    MACRO test_vert_line x, y1, y2
                        LD D,x
                        LD E,y1
                        LD L,y2
                        CALL draw2D.draw_vertical_line
                    ENDM

                    test_vert_line 225-8, 50-8, 50+8
                    test_vert_line 225-6, 50-6, 50+6
                    test_vert_line 225-4, 50-4, 50+4
                    test_vert_line 225-2, 50-2, 50+2
                    test_vert_line 225, 50, 50
                    test_vert_line 225+2, 50+2, 50-2
                    test_vert_line 225+4, 50+4, 50-4
                    test_vert_line 225+6, 50+6, 50-6
                    test_vert_line 225+8, 50+8, 50-8

                    DI
                    HALT


                    INCLUDE "mem.asm"
                    INCLUDE "muldiv.asm"
                    INCLUDE "misc.asm"
                    INCLUDE "draw2D.asm"
                    INCLUDE "draw2DEX.asm"
;                    INCLUDE "draw3D.asm"

code_size           EQU $-code


                    ALIGN 256
data
                    INCLUDE "screen_table.dat"
                    INCLUDE "muldiv.dat"
                    INCLUDE "sincos.dat"
;                    INCLUDE "draw2D.dat"
;                    INCLUDE "draw3D.dat"
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
