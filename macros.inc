                    MACRO sAto16 reg_hi, reg_lo
                        LD reg_lo,A
                        ADD A
                        SBC A
                        LD reg_hi,A
                    ENDM

                    MACRO neg16 reg_hi, reg_lo
                        XOR A
                        SUB reg_lo
                        LD reg_lo,A
                        SBC A
                        SUB reg_hi
                        LD reg_hi,A
                    ENDM

                    MACRO selop1 cond, op1, op2, arg
                        IF cond
                        op1 arg
                        ELSE
                        op2 arg
                        ENDIF
                    ENDM

                    MACRO selop2 cond, op1, op2, arg1, arg2
                        IF cond
                        op1 arg1,arg2
                        ELSE
                        op2 arg1,arg2
                        ENDIF
                    ENDM
