                    milestone
                    MODULE draw3D

x_center            EQU project_x.center1
y_center            EQU project_y.center1


set_viewport
; BC - x_center, y_center
; DE - x_min, y_min
; HL - x_max, y_max
; Preserves ALL except A.
                    LD A,B
                    LD (project_x.center1),A
                    LD (project_x.center2),A

                    LD A,C
                    LD (project_y.center1),A
                    LD (project_y.center2),A

                    JP draw2DEX.set_viewport


                    MACRO _draw3D_project coord
                    ; HL - x_or_y
                    ; DE - z
                    ; Output:
                    ; If coord == "x"
                    ;     HL = x_center + x * 256 / (z + 256)
                    ; If coord == "y"
                    ;     HL = y_center - y * 256 / (z + 256)
                    ; Special - returns!
                    ; Preserves IXH, IY, BC', DE', HL'.
                        LD A,H
                        ADD A
                        JP C,.negative

.i                      = 0
                        DUP 2
.i                      = .i+1

                            IF .i == 1
.positive
                                LD A,H
                                LD B,L
                                LD C,0
                                ; ABC = +x_or_y * 256
                            ELSE
.negative
                                XOR A
                                LD C,A
                                SUB L
                                LD B,A
                                SBC A
                                SUB H
                                ; ABC = -x_or_y * 256
                            ENDIF
                            ; ABC = abs(x_or_y) * 256

                            INC D ; DE = z + 256

                            CALL div_uABC_15bitDE_ABC ; TODO: this is suboptimal
                            ; ABC = abs(x_or_y) * 256 / (z + 256)

                            if_then_else coord=="x", <LD A,128>, <LD A,96>
                            LUA ALLPASS
                                sj.insert_label("draw3D.project_"..string.char(_c("coord"))..".center".._c(".i"), sj.current_address - 1)
                            ENDLUA
                            ; A = x_or_y_center

                            IF (coord=="x") == (.i == 1)
                                ADD C
                                LD L,A
                                ADC B
                                SUB L
                                LD H,A
                                ; HL = x_or_y_center + abs(x_or_y) * 256 / (z + 256)
                            ELSE
                                SUB C
                                LD L,A
                                SBC A
                                SUB B
                                LD H,A
                                ; HL = x_or_y_center - abs(x_or_y) * 256 / (z + 256)
                            ENDIF

                            RET
                        EDUP
                    ENDM


project_x
; HL - x
; DE - z
; Output:
; HL = x_center + x * 256 / (z + 256)
; Preserves IXH, IY, BC', DE', HL'.
                    _draw3D_project "x"


project_y
; HL - y
; DE - z
; Output:
; HL = y_center - y * 256 / (z + 256)
; Preserves IXH, IY, BC', DE', HL'.
                    _draw3D_project "y"

                    ENDMODULE
                    milestone
