    MODULE mem

; TODO - Review.

fill:
; B - count_256 (number of 256-byte blocks)
; D - value
; HL - dst_addr
; Output:
; HL=dst_addr+count_256*256
; Not modified:
; A,IX,IY,EX
; Side effects:
; B=C=0
; E=D
    LD (.old_sp),SP

    LD C,0
    ADD HL,BC
    LD SP,HL

    LD E,D
.loop:
    DUP 128
        PUSH DE
    EDUP
    DEC B
    JP NZ,.loop

    LD SP,(.old_sp)
    RET
.old_sp:
    WORD 0


copy:
; B - count_256 (number of 256-byte blocks)
; HL - src_addr
; DE - dst_addr (aligned 256)
; Output:
; HL=dst_addr+count_256*256
; Not modified:
; A,C,IX,IY,EX
; Side effects:
; B=0
    LD (.old_sp),SP

    LD SP,HL
    EX DE,HL

.loop:
    DUP 128
        POP DE
        LD (HL),E
        INC L
        LD (HL),D
        INC L
    EDUP

    INC H
    DEC B
    JP NZ,.loop

    LD SP,(.old_sp)
    RET
.old_sp:
    WORD 0

    ENDMODULE
