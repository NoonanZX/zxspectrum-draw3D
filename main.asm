                    DEVICE ZXSPECTRUM128

                    ORG #8000


                    DEFINE TEST

                    INCLUDE "config.inc"
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

                    LD BC,(3<<8)+(64+6)
                    LD HL,#5800
                    CALL mem.fill_blocks

                    LD BC,(3<<8)+(64+6)
                    LD HL,#D800
                    CALL mem.fill_blocks

                    XOR A
;                    LD A,2
                    OUT (254),A

                    LD BC,1000
.loop               PUSH BC

                    IFNDEF TEST
                    LD BC,24<<8
                    LD HL,screen
                    CALL mem.fill_blocks
                    ENDIF

                    CALL draw

                    IFNDEF TEST
;                    EI
;                    HALT
;                    DI

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

                    LD A,4
                    OUT (254),A

                    HALT


                    INCLUDE "demo.asm"
                    INCLUDE "mem.asm"
                    INCLUDE "muldiv.asm"
                    INCLUDE "sincos.asm"
                    INCLUDE "draw2D.asm"
                    INCLUDE "draw2DEX.asm"
                    INCLUDE "draw3D.asm"
code_size           EQU $-code

                    ALIGN 256
data                INCLUDE "screen_table.dat"
                    INCLUDE "muldiv.dat"
                    INCLUDE "sincos.dat"
                    INCLUDE "draw2D.dat"
                    INCLUDE "draw3D.dat"
data_size           EQU $-data

                    DISPLAY "Size: ",/D,$-code
                    DISPLAY "Code: ",/D,code_size
                    DISPLAY "Data: ",/D,data_size

                    SAVESNA "test.sna", code
                    SAVESNA "qsave1.sna", code
                    LABELSLIST "user.l"
;                    SAVESNA "c:/tools/unreal/qsave1.sna", code
;                    LABELSLIST "c:/tools/unreal/user.l"
