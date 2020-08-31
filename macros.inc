    MACRO neg8 reg
; reg - value
; Output:
; reg=-value
        XOR A
        SUB reg
        LD reg,A
    ENDM

    MACRO neg16 reg_hi,reg_lo
; reg_hi,reg_lo - value
; Output:
; reg_hi,reg_lo=-value
        XOR A
        SUB reg_lo
        LD reg_lo,A
        SBC A
        SUB reg_hi
        LD reg_hi,A
    ENDM

    MACRO minmax_step reg_min,reg_max
; A - value
; Output:
; reg_min=min(reg_max,value)
; reg_max=max(reg_max,value)
        CP reg_min
        JR C,2F ; if (value<reg_min) jump
        CP reg_max
        JR C,3F ; if (value<reg_max) jump
1:
        LD reg_max,A
        JP 3F
2:
        LD reg_min,A
3:
    ENDM
