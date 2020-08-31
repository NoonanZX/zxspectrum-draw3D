mul_DxE_HL:
; Unsigned multiplication ([0,255]*[0,255]->[0,65535])
; D - x
; E - y
; Output:
; HL=x*y
; Not modified:
; BC,IX,IY,EX
; Side effects:
; D=0
    LD H,D
    XOR A
    LD D,A
    LD L,A

    DUP 8
        ADD HL,HL
        JR NC,$+4
        ADD HL,DE
    EDUP

    RET


imul_BxC_HL:
; Signed multiplication ([-128,127]*[-128,127]->[-32768,32767])
; B - x
; C - y
; Output:
; HL - x*y
; Not modified:
; BC,IX,IY,EX
    LD H,HIGH(sqr_table)
    LD L,B
    LD E,(HL)
    INC H
    LD D,(HL) ; DE=x^2

    LD L,C
    LD A,(HL)
    DEC H
    LD L,(HL)
    LD H,A ; HL=y^2
    
    ADD HL,DE
    EX DE,HL ; DE=x^2+y^2

    LD A,B
    ADD C
    JP PE,.using_diff
.using_sum:
    LD H,HIGH(sqr_table)
    LD L,A
    LD A,(HL)
    INC H
    LD H,(HL)
    LD L,A ; HL=(x+y)^2

    OR A
    SBC HL,DE ; HL=(x+y)^2-x^2-y^2=2xy

    SRA H
    RR L ; HL=x*y

    RET
.using_diff:
    SUB C
    SUB C ; A=x-y

    LD H,HIGH(sqr_table)
    LD L,A
    LD A,(HL)
    INC H
    LD H,(HL)
    LD L,A ; HL=(x-y)^2

    EX DE,HL

    LD A,H
    OR A
    JP M,.special_case ; -128*-128*2=#8000 causes overflow

    SBC HL,DE ; HL=(x^2+y^2)-(x-y)^2=2xy

    SRA H
    RR L ; HL=x*y

    RET
.special_case:
    SRL H ; HL=x*y
    RET


fmul_BxC_A:
; Normalized real multiplication ([-1.0,+1.0]*[-1.0,+1.0]->[-1.0,+1.0])
; B - x
; C - y
; Output:
; A - x*y
; Not modified:
; BC,IX,IY,EX
    LD A,B
    OR A
    RET Z ; if (x==0) return 0

    LD A,C
    OR A
    RET Z ; if (y==0) return 0

    LD H,HIGH(sqr_table)
    LD L,B
    LD E,(HL)
    INC H
    LD D,(HL) ; DE=x^2

    LD L,C
    LD A,(HL)
    DEC H
    LD L,(HL)
    LD H,A ; HL=y^2
    
    ADD HL,DE
    EX DE,HL ; DE=x^2+y^2

    LD A,B
    ADD C
    JP PO,.using_sum
.using_diff:
    SUB C
    SUB C ; A=x-y

    LD H,HIGH(sqr_table)
    LD L,A
    LD A,(HL)
    INC H
    LD H,(HL)
    LD L,A ; HL=(x-y)^2

    EX DE,HL

    LD A,128 ; set sign bit - there was overflow
    JP .finish_calc
.using_sum:
    LD H,HIGH(sqr_table)
    LD L,A
    LD A,(HL)
    INC H
    LD H,(HL)
    LD L,A ; HL=(x+y)^2

    XOR A ; reset sign bit - there was no overflow
.finish_calc:
    OR A
    SBC HL,DE ; HL=2xy

    ADD B
    ADD C
    JP M,.adjust_dec

    LD A,B
    OR C
    LD A,H
    RET M
.adjust_inc: ; when x>0 and y>0
    INC A
    RET
.adjust_dec: ; when (x+y)<0
    LD A,H
    DEC A
    RET


div_BCxDE_BC:
; Unsigned divison
; BC - x
; DE - y
; Output:
; BC=x/y if y!=0 else 0
; HL=x%y if y!=0 else 0
; Not modified:
; IX,IY,EX
    XOR A
    LD L,A
    LD H,A
    SUB E
    LD E,A
    SBC A
    SUB D
    LD D,A

    LD A,B
    DUP 8
        RLA
        ADC HL,HL
        ADD HL,DE
        JR C,$+4
        SBC HL,DE
    EDUP
    RLA
    LD B,A

    LD A,C
    DUP 8
        RLA
        ADC HL,HL
        ADD HL,DE
        JR C,$+4
        SBC HL,DE
    EDUP
    RLA
    LD C,A

    RET
