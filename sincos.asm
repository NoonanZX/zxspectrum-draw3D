mul_cosA_sB_HL
; A - angle
; B - scalar (signed)
; Output:
; HL = cos(A) * B
; Preserves IX, IY, ALL'.
                    ADD 64
                    ; fall through to mul_sinA_sB_HL


mul_sinA_sB_HL
; A - angle
; B - scalar (signed)
; Output:
; HL = sin(A) * B
; Preserves IX, IY, ALL'.
                    LD H,HIGH(sin_table)
                    LD L,A
                    ADD 128
                    JP C,.angle_ge_pi
.angle_lt_pi
                    LD C,(HL)
                    JP mul_sB_uC_HL
.angle_ge_pi
                    LD L,A
                    LD C,(HL)

                    XOR A
                    SUB B
                    LD B,A
                    
                    JP mul_sB_uC_HL
