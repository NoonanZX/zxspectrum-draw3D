                    milestone
                    MODULE draw3D

; TODO: Projection and other transformation are rudimentary now.


position_x          EQU set_vertices.position_x
position_y          EQU set_vertices.position_y
position_z          EQU set_vertices.position_z

roll                EQU set_vertices.roll
pitch               EQU set_vertices.pitch
yaw                 EQU set_vertices.yaw


rotate
; TODO - make counterclockwise
; A - angle (clock-wise)
; IXL - x
; IYL - y
; Output:
; IXH = +x*cos(angle) + y*sin(angle)
; IYH = -x*sin(angle) + y*cos(angle)
; Preserves IXL, IYL, ALL'.
                    LD IYH,A ; IYH = angle

                    LD B,IXL ; x
                    CALL mul_cosA_sB_HL
                    PUSH HL

                    LD A,IYH ; angle
                    LD B,IYL ; y
                    CALL mul_sinA_sB_HL                 

                    POP DE
                    ADD HL,DE
                    LD A,H
                    LD IXH,A ; IXH = +x*cos(angle) + y*sin(angle)

                    LD A,128
                    ADD IYH ; angle + pi
                    LD B,IXL ; x
                    CALL mul_sinA_sB_HL
                    PUSH HL

                    LD A,IYH ; angle
                    LD B,IYL ; y
                    CALL mul_cosA_sB_HL

                    POP DE
                    ADD HL,DE
                    LD A,H
                    LD IYH,A ; IYH = -x*sin(angle) + y*cos(angle)

                    RET


set_vertices
; TODO: validate projected points.
; Transforms + project 'count' of 'vertices' and store results to internal buffer starting from 'index0'.
; B - count
; C - index0
; HL - vertices [x1, y1, z1...]
; Output:
; HL += 3 * count
; Preserves NOTHING.
                    ; loading
                    LD A,(HL)
                    LD IXL,A
                    INC HL
                    LD A,(HL)
                    LD IYL,A
                    INC HL
                    LD E,(HL)
                    INC HL
                    ; local = (IXL, IYL, E)

                    ; rotating around OZ (clock-wise)
                    EXX
                    LD A,0
.roll               EQU $-1
                    CALL rotate
                    EXX
                    ; (IXH, IYH, E)

                    ; rotating around OX (clock-wise)
                    LD D,IXH
                    LD IXL,E
                    LD IYL,IYH
                    LD A,0
.pitch              EQU $-1
                    EXX
                    CALL rotate
                    EXX
                    ; (D, IYH, IXH)

                    ; rotating around OY (clock-wise)
                    LD E,IYH
                    LD IXL,IXH
                    LD IYL,D
                    LD A,0
.yaw                EQU $-1
                    NEG
                    EXX
                    CALL rotate
                    EXX
                    LD A,E
                    ; rotated = (IYH, A, IXH)

                    LD IYL,C ; IYL = index (IXL is used by div_uABC_15bitDE_ABC)
                    EXX ; store BC, HL -1->

                    ; translating z
                    ; translating and projecting y

                    sAto16 B,C ; BC = y0 = 16bit(rotated.y)
                    LD A,IXH
                    sAto16 D,E ; DE = z0 = 16bit(rotated.z)

                    LD HL,0
.position_z         EQU $-2
                    OR A
                    ADC HL,DE
                    EX DE,HL
                    ; DE = z = z0 + position_z

                    LD HL,0
.position_y         EQU $-2
                    OR A
                    ADC HL,BC
                    ; HL = y = y0 + position_y

                    PUSH DE ; store z -2->

                    CALL .project_y
                    LD BC,96
                    ADD HL,BC
                    ; HL = y_projected = project(y, z) + 96

                    ; saving y_projected
                    LD D,HIGH(_vertex_screen_y)
                    LD E,IYL
                    EX DE,HL
                    LD (HL),E
                    INC H
                    LD (HL),D

                    ; translating and projecting x

                    LD A,IYH
                    sAto16 B,C ; BC = x0 = 16bit(rotated.x)

                    LD HL,0
.position_x         EQU $-2
                    OR A
                    ADC HL,BC
                    ; HL = x = x0 + position_x

                    POP DE ; restore z <-2-

                    CALL .project_x
                    LD BC,128
                    ADD HL,BC
                    ; HL = x_projected = project(x, z) + 128

                    ; saving x_projected
                    LD D,HIGH(_vertex_screen_x)
                    LD E,IYL
                    EX DE,HL
                    LD (HL),E
                    INC H
                    LD (HL),D

                    EXX ; restore BC, HL <-1-

                    INC C
                    DJNZ set_vertices
                    RET
.project_x
; TODO: Review.
; TODO: screen distance should be variable.
; TODO: round up division result?
; HL - x
; DE - z
; SF - sign(x)
; Output:
; HL = screen_x  = x * 256 / (z + 256)
; Preserves IXH, IY, BC', DE', HL'.
                    JP M,.x_lt_0
.x_ge_0
                    LD A,H
                    LD B,L
                    LD C,0
                    ; ABC = x

                    INC D ; DE = z + 256

                    CALL div_uABC_15bitDE_ABC ; TODO: this is suboptimal

                    LD H,B
                    LD L,C
                    ; HL = screen_x = ABC / DE

                    RET
.x_lt_0
                    XOR A
                    LD C,A
                    SUB L
                    LD B,A
                    SBC A
                    SUB H
                    ; ABC = -x

                    INC D ; DE = z + 256

                    CALL div_uABC_15bitDE_ABC ; TODO: this is suboptimal

                    XOR A
                    SUB C
                    LD L,A
                    SBC A
                    SUB B
                    LD H,A
                    ; HL = screen_x = -(ABC / DE)

                    RET
.project_y
; Same as project_x, but negates result.
; TODO: Review.
; TODO: screen distance should be variable.
; TODO: round up division result?
; HL - y
; DE - z
; SF - sign(y)
; Output:
; HL = screen_y  = -y * 256 / (z + 256)
; Preserves IXH, IY, BC', DE', HL'.
                    JP M,.y_lt_0
.y_ge_0
                    LD A,H
                    LD B,L
                    LD C,0
                    ; ABC = y

                    INC D ; DE = z + 256

                    CALL div_uABC_15bitDE_ABC ; TODO: this is suboptimal

                    XOR A
                    SUB C
                    LD L,A
                    SBC A
                    SUB B
                    LD H,A
                    ; HL = screen_y = -(ABC / DE)

                    RET
.y_lt_0
                    XOR A
                    LD C,A
                    SUB L
                    LD B,A
                    SBC A
                    SUB H
                    ; ABC = -y

                    INC D ; DE = z + 256

                    CALL div_uABC_15bitDE_ABC ; TODO: this is suboptimal

                    LD H,B
                    LD L,C
                    ; HL = screen_y = ABC / DE

                    RET


                    MACRO _draw3D_load_point_regs
                    ; L - index
                    ; Output:
                    ; BC - _vertex_screen_x[index]
                    ; DE - _vertex_screen_y[index]
                    ; Preserves A, L, IX, IY, ALL'.
                        LD H,HIGH(_vertex_screen_pos)
                        LD C,(HL)
                        INC H
                        LD B,(HL)
                        INC H
                        LD E,(HL)
                        INC H
                        LD D,(HL)
                    ENDM


                    MACRO _draw3D_load_point_stack
                    ; L - index
                    ; Output:
                    ; Stack += [_vertex_screen_x[index], _vertex_screen_y[index] <= top]
                    ; Preserves A, DE, L, IX, IY, ALL'.
                        LD H,HIGH(_vertex_screen_pos)
                        LD C,(HL)
                        INC H
                        LD B,(HL)
                        PUSH BC

                        INC H
                        LD C,(HL)
                        INC H
                        LD B,(HL)
                        PUSH BC
                    ENDM


draw_point
; Draws single point using 'index' vertex.
; L - index
; Preserves IX, IY, ALL'.
                    _draw3D_load_point_regs
                    JP draw2DEX.draw_point


draw_line
; Draws line between 'index1' and 'index2' vertices.
; H - index1
; L - index2
; Preserves IX, IY.
                    LD A,H
                    _draw3D_load_point_regs
                    EXX
                    LD L,A
                    _draw3D_load_point_regs

                    JP draw2DEX.draw_line


draw_polygon
; DE - [index1, index2 ... indexN, -1]
; Points are ordered counterclockwise.
; Must be at least 3 points.
; HL - pattern_8x8
; Preserves IY.
                    LD (.pattern),HL

                    LD BC,-1
                    PUSH BC

                    LD A,(DE)
                    LD L,A
                    _draw3D_load_point_stack
                    INC DE

                    LD A,(DE)
                    LD L,A
                    _draw3D_load_point_stack
                    INC DE

                    LD A,(DE)
                    LD L,A
.loop               _draw3D_load_point_stack
                    INC DE

                    LD A,(DE)
                    LD L,A
                    INC A
                    JP NZ,.loop

                    LD HL,0
.pattern            EQU $-2
                    CALL draw2DEX.draw_polygon

                    ENDMODULE
                    milestone
