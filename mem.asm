                    MODULE mem

fill_blocks
; Fills B 256-byte blocks at (HL) with C.
; Requires B > 0.
; Preserves A, IX, IY, ALL'.
                    LD D,C
                    LD E,C

                    LD (.old_sp),SP
                    LD C,0
                    ADD HL,BC
                    LD SP,HL

.loop
                    DUP 128
                        PUSH DE
                    EDUP
                    DEC B
                    JP NZ,.loop

                    LD SP,0
.old_sp             EQU $-2

                    RET


copy_blocks
; Copies B 256-byte blocks from (HL) to (DE).
; Requires E = 0, B > 0.
; Preserves A, C, IX, IY, ALL'.
                    LD (.old_sp),SP
                    LD SP,HL

                    EX DE,HL
.loop
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

                    LD SP,0
.old_sp             EQU $-2

                    RET

                    ENDMODULE
