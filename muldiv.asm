mul_uD_uE_HL
; Unsigned 8-bit multiplication.
; D - multiplier
; E - multiplicand
; Output:
; HL = product = multiplier * multiplicand
; Side effects:
; D = 0
; Preserves BC, IX, IY, ALL'.
                    LD H,D
                    XOR A
                    LD D,A
                    LD L,A

                    DUP 8
                        ADD HL,HL
                        JP NC,$+4
                        ADD HL,DE
                    EDUP

                    RET


mul_sB_sC_HL
; Signed 8-bit multiplication.
; B - multiplier
; C - multiplicand
; Output:
; HL = product = multiplier * multiplicand
; Preserves BC, IX, IY, ALL'.
                    LD H,HIGH(pow2s_table)
                    LD L,B
                    LD E,(HL)
                    INC H
                    LD D,(HL)
                    ; DE = B^2

                    LD L,C
                    LD A,(HL)
                    DEC H
                    LD L,(HL)
                    LD H,A
                    ; HL = C^2
    
                    ADD HL,DE
                    EX DE,HL
                    ; DE = B^2 + C^2

                    LD A,B
                    ADD C
                    JP PE,.using_diff
.using_sum
                    LD L,A
                    ; L = B + C

                    LD H,HIGH(pow2s_table)
                    LD A,(HL)
                    INC H
                    LD H,(HL)
                    LD L,A
                    ; HL = (B + C)^2

                    OR A
                    SBC HL,DE
                    ; HL = (B + C)^2 - (B^2 + C^2) = 2*B*C

                    SRA H
                    RR L
                    ; HL = B*C

                    RET
.using_diff
                    SUB C
                    SUB C
                    LD L,A
                    ; L = B - C

                    LD A,D
                    OR A
                    JP M,.special_case ; (B=-128)^2 + (C=-128)^2 = #8000 causes overflow
    
                    LD H,HIGH(pow2s_table)
                    LD A,(HL)
                    INC H
                    LD H,(HL)
                    LD L,A
                    ; HL = (B - C)^2

                    EX DE,HL
                    SBC HL,DE
                    ; HL = (B^2 + C^2) - (B - C)^2 = 2*B*C

                    SRA H
                    RR L
                    ; HL = B * C

                    RET
.special_case
                    EX DE,HL ; HL = #8000
                    SRL H ; HL = (B=-128) * (C=-128) = #4000

                    RET


mul_sB_uC_HL
; Signed-unsigned 8-bit multiplication.
; B - multiplier (signed)
; C - multiplicand (unsigned)
; Output:
; HL = product = multiplier * multiplicand
; Preserves BC, IX, IY, ALL'.
                    LD H,HIGH(pow2s_table)
                    LD L,B
                    LD E,(HL)
                    INC H
                    LD D,(HL)
                    ; DE = B^2

                    INC H ; H = HIGH(pow2u_table)
                    LD L,C
                    LD A,(HL)
                    INC H
                    LD H,(HL)
                    LD L,A
                    ; HL = C^2
    
                    ADD HL,DE
                    EX DE,HL
                    ; DE = B^2 + C^2

                    BIT 7,B
                    JP Z,.using_diff
.using_sum          ; B < 0
                    XOR A
                    SUB B
                    SUB C
                    JP NC,$+5
                    NEG
                    ; A = abs(B + C)

                    LD H,HIGH(pow2u_table)
                    LD L,A
                    LD A,(HL)
                    INC H
                    LD H,(HL)
                    LD L,A
                    ; HL = (B + C)^2

                    OR A
                    SBC HL,DE
                    ; HL = (B + C)^2 - (B^2 + C^2) = 2*B*C
                    RET Z

                    SRL H
                    RR L
                    SET 7,H
                    ; HL = B*C

                    RET
.using_diff         ; B >= 0
                    LD A,B
                    SUB C
                    JP NC,$+5
                    NEG
                    ; A = abs(B - C)

                    LD H,HIGH(pow2u_table)
                    LD L,A
                    LD A,(HL)
                    INC H
                    LD H,(HL)
                    LD L,A
                    ; HL = (B - C)^2

                    EX DE,HL
                    OR A
                    SBC HL,DE
                    ; HL = (B^2 + C^2) - (B - C)^2 = 2*B*C

                    SRL H
                    RR L
                    ; HL = B * C

                    RET


                    MACRO _div_init
                        XOR A
                        LD L,A
                        LD H,A
                        SUB E
                        LD E,A
                        SBC A
                        SUB D
                        LD D,A
                    ENDM

                    MACRO _div_block_no_overflow
                        DUP 8
                            RLA
                            ADC HL,HL
                            ADD HL,DE
                            JP C,$+5
                            SBC HL,DE
                        EDUP
                        RLA
                    ENDM

                    MACRO _div_block_can_overflow
                        DUP 8
                            RLA
                            ADC HL,HL
                            JP C,$+7
                            ADD HL,DE
                            JP $+5
                            ADD HL,DE
                            SCF
                            JP C,$+5
                            SBC HL,DE
                        EDUP
                        RLA
                    ENDM


div_uBC_uDE_BC
; Unsigned 16-bit division.
; BC - divident
; DE - divisor
; Output:
; BC = quotient = divident / divisor (if divisor > 0)
; BC = 0 (if divisor is 0)
; Preserves IX, IY, ALL'.
                    _div_init
                    LD A,B
                    _div_block_no_overflow
                    LD B,A
                    LD A,C
                    _div_block_no_overflow
                    LD C,A
                    RET


div_uABC_uDE_ABC
; Unsigned 24-bit / 16-bit division.
; ABC - divident
; DE - divisor
; Output:
; ABC = quotient = divident / divisor (if divisor > 0)
; ABC = 0 (if divisor is 0)
; Preserves IXH, IY, BC', DE', HL'.
                    BIT 7,D
                    JP Z,div_uABC_15bitDE_ABC
                    EX AF,AF'
                    _div_init
                    EX AF,AF'
                    _div_block_can_overflow
                    LD IXL,A
                    LD A,B
                    _div_block_can_overflow
                    LD B,A
                    LD A,C
                    _div_block_can_overflow
                    LD C,A
                    LD A,IXL
                    RET


div_uABC_15bitDE_ABC
; Unsigned 24-bit / 15-bit division.
; Faster than div_uABC_uDE_ABC, but divisor should be <= 32768.
; ABC - divident
; DE - divisor
; Output:
; ABC = quotient = divident / divisor (if divisor > 0)
; ABC = 0 (if divisor is 0)
; Preserves IXH, IY, BC', DE', HL'.
                    EX AF,AF'
                    _div_init
                    EX AF,AF'
                    _div_block_no_overflow
                    LD IXL,A
                    LD A,B
                    _div_block_no_overflow
                    LD B,A
                    LD A,C
                    _div_block_no_overflow
                    LD C,A
                    LD A,IXL
                    RET
