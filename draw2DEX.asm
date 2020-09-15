                    MODULE draw2DEX

; Viewpoint (inclusive).
x_min               EQU test_point.x_min ; default = 0
y_min               EQU test_point.y_min ; default = 0
x_max               EQU test_point.x_max ; default = 255
y_max               EQU test_point.y_max ; default = 191


set_viewport
; BC - x_min, y_min
; DE - x_max, y_max
; Preserves ALL except A.
                    LD A,B
                    LD (x_min),A                    
                    LD A,C
                    LD (y_min),A
                    LD A,D
                    LD (x_max),A
                    LD A,E
                    LD (y_max),A
                    RET


test_point
; Tests if point is within viewport.
; BC - x
; DE - y
; Output:
; CF = (x < x_min) || (x > x_max) || (y < y_min) || (y > y_max)
; Preserves ALL except A.
                    XOR A
                    CP B
                    RET C ; CF = (x < 0) || (x > 255)
                    CP D
                    RET C ; CF = (y < 0) || (y > 255)

                    LD A,191
.y_max              EQU $-1
                    CP E
                    RET C ; CF = (y > y_min)

                    LD A,E
                    CP 0
.y_min              EQU $-1
                    RET C ; CF = (y < y_min)

                    LD A,255
.x_max              EQU $-1
                    CP C
                    RET C ; CF = (x > x_min)

                    LD A,C
                    CP 0
.x_min              EQU $-1
                    RET ; CF = (x < x_min)


clip_line
; Clips line |(x1, y1) - (x2, y2)| against viewport.
; BC - x1
; DE - y1
; BC' - x2
; DE' - y2
; Output:
; |(BC,  DE) - (BC', DE')| - part of line that is inside viewport,
; or
; CF = 1 if line is outside of viewport
                    EXX
                    CALL clip_line_begin
                    RET C
                    EXX
                    ; fall through to clip_line_begin


clip_line_begin
; BC - x1
; DE - y1
; BC' - x2
; DE' - y2
; Output:
; BC, DE = new_x, new_y - first point on line |(x1, y1) - (x2, y2)| inside viewport, or
; CF = 1 if not found
; Preserves BC', DE'.
                    CALL test_point
                    RET NC
clip_line_begin_nocheck
; Can produce erroneous result if (x1, y1) is inside viewport.
                    PUSH DE
                    EXX
                    POP HL
                    OR A
                    SBC HL,DE
                    ; HL = y1 - y2

                    EX AF,AF' ; store flags

                    PUSH BC
                    EXX
                    POP HL
                    OR A
                    SBC HL,BC
                    ; HL = x2 - x1

                    MACRO _clip_line_begin_AtoHL sign
                    ; HL = A
                    ; CF = 0
                        IF sign >= 0
                            LD L,A
                            XOR A
                            LD H,A
                        ELSE
                            NEG
                            LD L,A
                            SBC A
                            LD H,A
                            OR A
                        ENDIF
                    ENDM

                    MACRO _clip_line_begin_HLtoA sign, on_overflow
                    ; A = sign * HL (if result is not in [0, 255] jumps to on_overflow)
                        IF sign >= 0
                            XOR A
                            CP H
                            JP NZ,on_overflow
                            LD A,L
                        ELSE
                            LD A,255
                            CP H
                            JP NZ,on_overflow
                            LD A,L
                            CPL
                            INC A
                            JP Z,on_overflow
                        ENDIF
                    ENDM

                    MACRO _clip_line_begin_adcsbc sign, arg1, arg2
                        ; arg1 += sign * args
                        IF sign >= 0
                        ADC arg1, arg2
                        ELSE
                        SBC arg1, arg2
                        ENDIF
                    ENDM

                    MACRO _clip_line_begin dir_x, dir_y
                    ; dir_x = sign(x2 - x1) if (x1 != x2) else +/-1
                    ; dir_y = sign(y2 - y1) if (y1 != y2) else +/-1
                    ; Input/Output same as clip_line_begin, plus:
                    ; HL - (x2 - x1)
                    ; HL' - (y1 - y2)
                    ; Special: Makes RET!
                        _clip_line_begin_HLtoA +dir_x, .split_line
                        LD IXL,A ; IXL = abs(x1 - x2)
                        EXX
                        _clip_line_begin_HLtoA -dir_y, .split_line_reverse
                        LD IXH,A ; IXH = abs(y1 - y2)
                        EXX

                        LD IY,0

                        ; comparing x1 agains farest viewport border
                        IF dir_x > 0
                        LD A,(x_max)
                        ELSE
                        LD A,(x_min)
                        ENDIF
                        _clip_line_begin_AtoHL dir_x
                        _clip_line_begin_adcsbc -dir_x, HL, BC
                        JP M,.outside_of_viewport

                        ; calculating x_to_border = distance from x1 to nearest viewport border
                        IF dir_x > 0
                        LD A,(x_min)
                        ELSE
                        LD A,(x_max)
                        ENDIF
                        _clip_line_begin_AtoHL dir_x
                        _clip_line_begin_adcsbc -dir_x, HL, BC
                        JP M,1F ; if (x_to_border < 0) jump

                        _clip_line_begin_HLtoA +1, .outside_of_viewport
                        LD IYL,A ; IYL = x_to_border

                        LD A,IXL
                        SUB IYL
                        RET C ; if abs(x1 - x2) < x_to_border jump
1
                        ; comparing y1 agains farest viewport border
                        IF dir_y > 0
                        LD A,(y_max)
                        ELSE
                        LD A,(y_min)
                        ENDIF
                        _clip_line_begin_AtoHL dir_y
                        _clip_line_begin_adcsbc -dir_y, HL, DE
                        JP M,.outside_of_viewport

                        ; calculating y_to_border = distance from y1 to nearest viewport border
                        IF dir_y > 0
                        LD A,(y_min)
                        ELSE
                        LD A,(y_max)
                        ENDIF
                        _clip_line_begin_AtoHL dir_y
                        _clip_line_begin_adcsbc -dir_y, HL, DE
                        JP M,2F ; if (y_to_border < 0) jump

                        _clip_line_begin_HLtoA +1, .outside_of_viewport
                        LD IYH,A ; IYH = y_to_border

                        LD A,IXH
                        SUB IYH
                        RET C ; if abs(y1 - y2) < y_to_border jump
2
                        PUSH BC
                        PUSH DE
                        LD B,IYL ; x_to_border
                        LD C,IYH ; y_to_border
                        LD D,IXL ; abs(x1 - x2)
                        LD E,IXH ; abs(y1 - y2)
                        CALL .intersect_with_corner
                        POP DE
                        POP BC
                        RET C ; return if not found
                        ; H = offset_x = new_x - x1
                        ; L = offset_y = new_y - y1

                        LD A,H ; offset_x

                        LD H,0
                        IF dir_y > 0
                            ADD HL,DE
                        ELSE
                            EX DE,HL
                            SBC HL,DE
                            OR A
                        ENDIF
                        EX HL,DE
                        ; y1 += offset_y

                        IF dir_x > 0
                            LD H,0
                            LD L,A
                            ADD HL,BC
                        ELSE
                            LD H,B
                            LD L,C
                            LD B,0
                            LD C,A
                            SBC HL,BC
                            OR A
                        ENDIF
                        LD B,H
                        LD C,L
                        ; x1 += offet_x

                        RET
                    ENDM

                    JP M,.x1_gt_x2
.x1_le_x2           EX AF,AF'
                    JP P,.x1_le_x2_y1_ge_y2
.x1_le_x2_y1_lt_y2  _clip_line_begin +1, +1
.x1_le_x2_y1_ge_y2  _clip_line_begin +1, -1
.x1_gt_x2           EX AF,AF'
                    JP P,.x1_gt_x2_y1_ge_y2
.x1_gt_x2_y1_lt_y2  _clip_line_begin -1, +1
.x1_gt_x2_y1_ge_y2  _clip_line_begin -1, -1

.intersect_with_corner
; TODO: Suboptimal.
; Search intersection of line |(0, 0) - (dx, dy)| with corner |(xc, +inf) - (xc, yc) - (+inf, yc)|,
; i.e. first line point with {x >= xc, y >= yc}.
; BC - xc, yc
; DE - dx, dy
; Output:
; HL = x, y or
; CF = 1 if not found
                    LD HL,0 ; x = y = 0
                    LD A,D
                    CP E ; dx <=> dy
                    JP C,.by_y
.by_x               ; dx >= dy
                    RRA ; A = dx / 2
                    JP $+3
1                   EX AF,AF'
                    ADD E ; A += dy
                    JR C,2F ; if (A > 256) jump
                    CP D
                    JR C,3F ; if (A < dx) jump
2                   SUB D ; A -= dx
                    INC L ; y += 1
3                   INC H ; x += 1
                    EX AF,AF'
                    LD A,L
                    CP C ; y <=> yc
                    LD A,H
                    JP C,4F ; if (y < yc) jump
                    CP B ; x <=> xc
                    RET NC ; if (x >= xc) return
4                   CP D ; x <=> dx
                    JP C,1B ; if (x < dx) jump
                    CCF ; CF = 1
                    RET ; not found
.by_y               ; dx < dy
                    SUB D
                    ADD E
                    RRA ; A = dy / 2
                    JP $+3
1                   EX AF,AF'
                    ADD D ; A += dx
                    JR C,2F ; if (A > 256) jump
                    CP E
                    JR C,3F ; if (A < dy) jump
2                   SUB E ; A -= dy
                    INC H ; x += 1
3                   INC L ; y += 1
                    EX AF,AF'
                    LD A,H
                    CP B ; x <=> xc
                    LD A,L
                    JP C,4F ; if (x < xc) jump
                    CP C ; y <=> yc
                    RET NC ; if (y >= yc) return
4                   CP E ; y <=> dy
                    JP C,1B ; if (y < dy) jump
                    CCF ; CF = 1
                    RET ; not found

.split_line_reverse EXX
.split_line
; HL - (x2 - x1)
; HL' - (y1 - y2)
                    ; let xm = (x1 + x2) / 2
                    ; let ym = (y1 + y2) / 2
                    OR A
                    SRA H
                    RR L
                    ADD HL,BC
                    PUSH HL
                    ; Stack: xm

                    EXX

                    OR A
                    SRA H
                    RR L
                    ADD HL,DE
                    ; HL = ym

                    EX HL,DE
                    EX (SP),HL
                    ; DE = ym
                    ; HL = xm
                    ; Stack: y2

                    PUSH BC
                    LD B,H
                    LD C,L
                    ; BC = xm
                    ; Stack: y2, x2 <= top

                    EXX

                    ; BC = x1
                    ; DE = y1
                    ; BC' = xm
                    ; DE' = ym
                    ; Stack: y2, x2 <= top
                    CALL clip_line_begin_nocheck

                    JP C,$+4
                    EXX
                    POP BC
                    POP DE
                    EXX
                    ; On success:
                    ; BC  = new_x
                    ; DE  = new_y
                    ; On failure:
                    ; BC  = xm
                    ; DE  = ym
                    ; Always:
                    ; BC' = x2
                    ; DE' = y2

                    RET NC ; on success
                    JP clip_line_begin_nocheck
.outside_of_viewport
                    OR A
                    CCF
                    RET


draw_point
; BC - x
; DE - y
; Preserves IX, IY, EX.
                    CALL test_point
                    RET C

                    LD D,C
                    JP draw2D.draw_point


draw_line
; BC - x1
; DE - y1
; BC' - x2
; DE' - y2
                    CALL test_point
                    CALL C,clip_line_begin_nocheck
                    RET C

                    EXX

                    CALL test_point
                    CALL C,clip_line_begin_nocheck
                    ; no need to check clip result twice for same line

                    ; conversion to 8-bit coords
                    LD D,C
                    PUSH DE
                    EXX
                    LD D,C
                    POP HL

                    JP draw2D.draw_line

                    ENDMODULE
