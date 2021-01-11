                    MACRO sAto16 reg_hi, reg_lo
                        LD reg_lo,A
                        ADD A
                        SBC A
                        LD reg_hi,A
                    ENDM


                    MACRO if_then pred, cmd
                        IF pred
                            cmd
                        ENDIF
                    ENDM


                    MACRO if_then_else pred, cmd1, cmd2
                        IF pred
                            cmd1
                        ELSE
                            cmd2
                        ENDIF
                    ENDM
