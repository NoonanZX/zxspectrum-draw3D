                    milestone
                    MODULE draw2D

draw_point
; DE - x, y
; Preserves IX, IY, ALL'.
                    LD H,HIGH(screen_table)
                    LD L,E
                    LD C,(HL)
                    INC H
                    LD B,(HL)
                    ; BC = line_address[y]

                    LD HL,.masks
                    LD A,7
                    AND D
                    LD E,A ; E = x & 7
                    ADD L
                    LD L,A
                    ; HL = &mask = &masks[x & 7]

                    LD A,D
                    XOR E
                    RRA
                    RRA
                    RRA
                    ADD C
                    LD C,A
                    ; BC = pixel_address = line_address[y] + (x >> 3)

                    LD A,(BC)
                    OR (HL)
                    LD (BC),A
                    ; (pixel_address) |= mask

                    RET

                    ALIGN 8
.masks              BYTE %10000000, %01000000, %00100000, %00010000, %00001000, %00000100, %00000010, %00000001


draw_line
; DE - x1, y1
; HL - x2, y2
; Preserves IX, IY.
                    LD A,L
                    SUB E
                    JP C,.exchange
                    JP Z,draw_horizontal_line
                    JP 1F

.exchange           NEG
                    EX DE,HL

1                   LD C,A ; store C = y2 - y1

                    LD A,H
                    SUB D
                    JP C,.right_to_left
                    JP Z,draw_vertical_line.nocheck

                    MACRO _draw2D_draw_line_load_pixel_address
                    ; H - HIGH(screen_table)
                    ; L - y
                    ; B - shift
                    ; Output:
                    ; DE = pixel_address = line_address[y] + shift
                        LD A,(HL)
                        ADD B
                        LD E,A
                        INC H
                        LD D,(HL)
                        DEC H
                    ENDM

                    MACRO _draw2D_draw_line_draw_pixel
                    ; DE - pixel_address
                    ; C - mask
                    ; Output:
                    ; (pixel_address) |= mask
                        LD A,(DE)
                        OR C
                        LD (DE),A
                    ENDM

                    MACRO _draw2D_draw_line_init
                    ; H - dx = abs(x2 - x1)
                    ; L - dy = y2 - y1
                    ; D - x = x1
                    ; E - y = y1
                    ; Output:
                    ; B' = shift = x >> 3
                    ; C' = mask = masks[x & 7]
                    ; HL' = pixel_address = line_address[y] + shift
                    ; (pixel_address) |= mask - writes first pixel
                        LD A,E ; A = y
                        EX AF,AF'
                        LD A,D ; A = x
                        EXX
                            LD B,A
                            AND 7
                            LD C,A
                            ; B = x
                            ; C = x & 7

                            LD HL,draw_point.masks
                            ADD L
                            LD L,A
                            ; HL = &masks[x & 7]

                            LD A,B
                            XOR C
                            RRA
                            RRA
                            RRA
                            LD B,A
                            LD C,(HL)
                            ; B = x >> 3
                            ; C = masks[x & 7]

                            EX AF,AF' ; A = y

                            LD H,HIGH(screen_table)
                            LD L,A
                            ; HL = line_address[y]

                            _draw2D_draw_line_load_pixel_address
                            _draw2D_draw_line_draw_pixel
                        EXX
                    ENDM

                    MACRO _draw2D_draw_line_by_x dir_x
                    ; dir_x - sign(x2 - x1)
                    ; H - dx = abs(x2 - x1)
                    ; L - dy = y2 - y1
                    ; D - x = x1
                    ; E - y = y1
                    ; Special - returns!
                        _draw2D_draw_line_init
                        LD A,H
                        RRA ; A = dx / 2
                        LD B,H ; B = counter = dx
.loop                   ADD L ; A += dy
                        JR C,.step_y ; if (A >= 256) jump
                        CP H
                        JR NC,.step_y ; if (A >= dx) jump
                        EXX
                            EX AF,AF'
.step_x                     if_then_else dir_x>0, RRC C, RLC C ; RRC/RLC mask
                            JP NC,1F
                            if_then_else dir_x>0, INC B, DEC B ; shift +/-= 1
                            if_then_else dir_x>0, INC E, DEC E ; pixel_address +/-= 1
1                           _draw2D_draw_line_draw_pixel
                            EX AF,AF'
                        EXX
                        DJNZ .loop
                        RET
.step_y                 SUB H ; A -= dx
                        EXX
                            EX AF,AF'
                            INC L ; y += 1
                            _draw2D_draw_line_load_pixel_address
                            JP .step_x
                    ENDM

                    MACRO _draw2D_draw_line_by_y dir_x
                    ; dir_x - sign(x2 - x1)
                    ; H - dx = abs(x2 - x1)
                    ; L - dy = y2 - y1
                    ; D - x = x1
                    ; E - y = y1
                    ; Special - returns!
                        _draw2D_draw_line_init
                        LD A,L
                        RRA ; A = dy / 2
                        LD B,L ; B = counter = dy
.loop                   ADD H ; A += dx
                        JR C,.step_x ; if (A >= 256) jump
                        CP L
                        JR NC,.step_x ; if (A >= dy) jump
                        EXX
                            EX AF,AF'
.step_y                     INC L ; y += 1
                            _draw2D_draw_line_load_pixel_address
                            _draw2D_draw_line_draw_pixel
                            EX AF,AF'
                        EXX
                        DJNZ .loop
                        RET
.step_x                 SUB L ; A -= dy
                        EXX
                            EX AF,AF'
                            if_then_else dir_x>0, RRC C, RLC C ; RRC/RLC mask
                            JP NC,.step_y
                            if_then_else dir_x>0, INC B, DEC B ; shift +/-= 1
                            JP .step_y
                    ENDM
.left_to_right
                    LD H,A
                    LD L,C
                    ; H = dx = x2 - x1
                    ; L = dy = y2 - y1

                    SCF
                    SBC C
                    JP C,.left_to_right_by_y
.left_to_right_by_x _draw2D_draw_line_by_x +1
.left_to_right_by_y _draw2D_draw_line_by_y +1

.right_to_left
                    CPL
                    INC A
                    LD H,A
                    LD L,C
                    ; H = dx = x1 - x2
                    ; L = dy = y2 - y1

                    SBC C
                    JP C,.right_to_left_by_y
.right_to_left_by_x _draw2D_draw_line_by_x -1
.right_to_left_by_y _draw2D_draw_line_by_y -1


draw_horizontal_line
; D - x1
; H - x2
; E - y
; Preserves IX, IY, ALL'.
                    LD A,D
                    CP H
                    JP C,.nocheck
                    JP Z,draw_point
                    EX DE,HL
                    LD E,L
.nocheck            ; x1 <= x2.
                    LD B,H
                    ; D = x1
                    ; B = x2

                    LD HL,.r_masks
                    LD A,7
                    AND B
                    ADD L
                    LD L,A
                    LD C,(HL)
                    ; C = r_mask = r_masks[x2 & 7]

                    LD HL,.l_masks
                    LD A,7
                    AND D
                    ADD L
                    LD L,A
                    LD A,E
                    LD E,(HL)
                    ; A = y
                    ; E = l_mask = l_masks[x1 & 7]

                    LD L,A
                    LD H,HIGH(screen_table)
                    LD A,(HL)
                    INC H
                    LD H,(HL)
                    LD L,A
                    ; HL = line_address[y]

                    LD A,7
                    AND D
                    XOR D
                    RRA
                    RRA
                    RRA
                    LD D,A
                    ; D = x1 >> 3

                    ADD L
                    LD L,A
                    ; HL = pixel_address = line_address[y] + (x1 >> 3)

                    LD A,7
                    AND B
                    XOR B
                    RRA
                    RRA
                    RRA
                    SUB D
                    ; A = (x2 >> 3) - (x1 >> 3)

                    JR Z,.short
.long
                    LD B,A

                    LD A,E
                    OR (HL)
                    LD (HL),A
                    INC L
                    ; (pixel_address++) |= l_mask

                    DEC B
                    JR Z,2F

                    LD A,255
1                   LD (HL),A
                    INC L
                    ; (pixel_address++) = 255
                    DJNZ 1B

2                   LD A,C
                    OR (HL)
                    LD (HL),A
                    ; (pixel_address) |= r_mask

                    RET
.short
                    LD A,C
                    AND E
                    OR (HL)
                    LD (HL),A
                    ; (pixel_address) |= (l_mask & r_mask)

                    RET

                    ALIGN 8
.l_masks            BYTE %11111111, %01111111, %00111111, %00011111, %00001111, %00000111, %00000011, %00000001
.r_masks            BYTE %10000000, %11000000, %11100000, %11110000, %11111000, %11111100, %11111110, %11111111


draw_vertical_line
; D - x
; E - y1
; L - y2
; Preserves IX, IY, ALL'.
                    LD A,E
                    CP L
                    JP C,.nocheck
                    JP Z,draw_point
                    EX DE,HL
                    LD D,H
.nocheck            ; y1 <= y2.
                    LD A,L
                    SUB E
                    INC A
                    LD B,A
                    ; B = y2 - y1 + 1

                    LD A,7
                    AND D
                    LD C,A
                    ; C = x & 7

                    LD HL,draw_point.masks
                    ADD L
                    LD L,A
                    ; HL = &masks[x & 7]

                    LD A,D
                    XOR C
                    RRA
                    RRA
                    RRA
                    LD D,A
                    ; D = x >> 3

                    LD C,(HL)
                    ; C = mask = masks[x & 7]

1                   LD H,HIGH(screen_table)
                    LD L,E
                    LD A,(HL)
                    ADD D
                    INC H
                    LD H,(HL)
                    LD L,A
                    ; HL = pixel_address = line_address[y] + (x >> 3)

                    LD A,(HL)
                    OR C
                    LD (HL),A
                    ; (pixel_address) |= mask

                    INC E
                    DJNZ 1B

                    RET


draw_polygon
; Stack - [-1, (x1, y1), (x2, y2) ... (xn, yn) <= top]
; Points are ordered counterclockwise.
; Must be at least 3 points.
; HL - pattern_8x8
; Preserves DE, IXH, IY.
                    POP BC
                    EXX

                    POP DE
                    LD (.last_point),DE

                    POP HL
                    CALL convex.add_line

                    POP HL
.loop               CALL convex.add_line

                    POP HL

                    LD A,L
                    INC A
                    JP NZ,.loop ; if L <> -1 jump

                    LD HL,0
.last_point         EQU $-2
                    CALL convex.add_line

                    EXX
                    PUSH BC
                    JP convex.fill


                    MODULE convex
                    ; To draw convex figure - define border with add_xxx routines, then call fill.
                    ; Note that border must be defined clockwise, opposed to draw_polygon in which points are counterclockwise ordered.

add_vertical_line
; DE - x, y1
; HL - x, y2
; D = H
; Output:
; DE = x, y2
; Preserves D, IX, IY, ALL'.
                    LD A,L
                    SUB E
                    RET Z
                    EX DE,HL
                    JP C,.y1_gt_y2
.y1_lt_y2
                    LD B,A
                    LD H,HIGH(_x_max)

                    LD C,L

1                   LD (HL),D
                    INC L
                    DJNZ 1B
                    LD (HL),D

                    LD H,L
                    LD L,C
                    JP _add_max_interval
.y1_gt_y2
                    NEG
                    LD B,A
                    LD H,HIGH(_x_min)

                    LD C,L

1                   LD (HL),D
                    DEC L
                    DJNZ 1B
                    LD (HL),D

                    LD H,C
                    JP _add_min_interval


add_line
; DE - x1, y1
; HL - x2, y2
; Output:
; DE - x2, y2
; Preserves IX, IY, BC', DE', HL'.
                    LD A,H
                    SUB D
                    JP Z,add_vertical_line ; if x1 == x2 jump

                    EX AF,AF'

                    LD A,L
                    SUB E
                    EX DE,HL
                    RET Z ; if y1 == y2 return

                    MACRO _draw2D_convex_add_line dir_x, dir_y
                    ; dir_x = sign(x2 - x1)
                    ; dir_y = sign(y2 - y1)
                    ; D - dx = abs(x1 - x2)
                    ; E - dy = abs(y1 - y2)
                    ; HL - x1, y1
                    ; Output:
                    ; D = x1 + dx
                    ; E = y1 + dy
                    ; Preserves IX, IY, ALL'.
                    ; Special - returns!
                        LD C,H   ; (C, L) = (x, y)

                        if_then_else dir_y>0, <LD H,HIGH(_x_max)>, <LD H,HIGH(_x_min)>

                        LD A,E
                        CP D
                        JP C,.by_x ; if dx > dy jump
.by_y
                        LD B,A ; counter = dy
                        RRA ; A = dy / 2
                        LD (HL),C
1                       ADD D ; A += dx
                        JR C,2F ; if (A >= 256) jump
                        CP E
                        JR C,3F ; if (A < dy) jump
2                       SUB E ; A -= dy
                        if_then_else dir_x>0, INC C, DEC C ; x +/-= 1
3                       if_then_else dir_y>0, INC L, DEC L ; y +/-= 1
                        LD (HL),C
                        DJNZ 1B

                        JP .done
.by_x
                        SUB E
                        ADD D
                        ; A = D = dx
                        ; CF = 0

                        LD B,A ; counter = dx
                        IF dir_y > 0
                            RRA ; A = dx / 2
                        ELSE
                            ; A = (dx + 1) / 2 - 1
                            ; This hack forces algorithm to iterate over exactly same dots when processing
                            ; line (x1, y1) - (x2, y2) and inverted line (x2, y2) - (x1, y1).
                            ; A will never became negative, because {dx > dy > 0} => {dx >= 2} => {A >= 0}).
                            INC A
                            RRA
                            DEC A
                        ENDIF
                        IF dir_x == dir_y
1                           ADD E ; A += dy
                            JR C,2F ; if (A >= 256) jump
                            CP D
                            JR C,3F ; if (A < dx) jump
2                           SUB D ; A -= dx
                            LD (HL),C
                            if_then_else dir_y>0, INC L, DEC L ; y +/-= 1
3                           if_then_else dir_x>0, INC C, DEC C ; x +/-= 1
                            DJNZ 1B
                            LD (HL),C
                        ELSE
                            LD (HL),C
1                           if_then_else dir_x>0, INC C, DEC C ; x +/-= 1
                            ADD E ; A += dy
                            JR C,2F ; if (A >= 256) jump
                            CP D
                            JR C,3F ; if (A < dx) jump
2                           SUB D ; A -= dx
                            if_then_else dir_y>0, INC L, DEC L ; y +/-= 1
                            LD (HL),C
3                           DJNZ 1B
                        ENDIF
.done
                        LD A,L
                        if_then_else dir_y>0, SUB E, ADD E

                        LD D,C
                        LD E,L

                        if_then dir_y>0, <LD H,L>
                        if_then_else dir_y>0, <LD L,A>, <LD H,A>
                        if_then_else dir_y>0, JP _add_max_interval, JP _add_min_interval
                    ENDM

                    JP C,.y1_gt_y2
.y1_lt_y2           LD E,A
                    EX AF,AF'
                    JP C,.y1_lt_y2_x1_gt_x2
.y1_lt_y2_x1_lt_x2  LD D,A
                    _draw2D_convex_add_line +1, +1
.y1_lt_y2_x1_gt_x2  NEG
                    LD D,A
                    _draw2D_convex_add_line -1, +1
.y1_gt_y2           NEG
                    LD E,A
                    EX AF,AF'
                    JP C,.y1_gt_y2_x1_gt_x2
.y1_gt_y2_x1_lt_x2  LD D,A
                    _draw2D_convex_add_line +1, -1
.y1_gt_y2_x1_gt_x2  NEG
                    LD D,A
                    _draw2D_convex_add_line -1, -1


                    ; Each half of convex shape border, both left (x_min) and right (x_max) has its own [y_min, y_max] interval.
                    ; It can consist of one solid part (example - y2-y3-y4-y5), or be separated into two connecting parts (example - y4-y5-yn | yn-y1-y2).
_xmin_yminmax_part1 EQU fill.xmin_yminmax_part1
_xmin_yminmax_part2 EQU fill.xmin_yminmax_part2
_xmax_yminmax_part1 EQU fill.xmax_yminmax_part1
_xmax_yminmax_part2 EQU fill.xmax_yminmax_part2

_interval_empty     EQU #FDFF
_interval_bad       EQU #FCFE


                    MACRO _draw2D_convex_try_save_interval dir, addr
                    ; Tries to store/append interval HL to (addr).
                    ; dir - intervals directions:
                    ;     If dir > 0
                    ;         [begin(HL), end(HL)] = [L, H]
                    ;         [begin( (addr) ), end( (addr) )] = [(addr), (addr+1)]
                    ;     If dir < 0
                    ;         [begin(HL), end(HL)] = [H, L]
                    ;         [begin( (addr) ), end( (addr) )] = [(addr+1), (addr)]
                    ; HL - interval
                    ; Output:
                    ; If (addr) == _interval_empty
                    ;     (addr) = HL
                    ;     return
                    ; Elif end( (addr) ) == begin(HL)
                    ;     (addr) = [begin( (addr) ), end(HL)]
                    ;     return
                    ; Else
                    ;     Fall through...
                    ; Preserves ALL except AF.
                        if_then_else dir<0, <LD A,(addr)>, <LD A,(addr+1)>
                        if_then_else dir<0, CP LOW(_interval_empty), CP HIGH(_interval_empty)
                        JR NZ,.add ; if (addr) == _interval_empty jump
.new
                        LD (addr),HL
                        RET
.add
                        if_then_else dir<0, CP H, CP L
                        JR NZ,.failed ; if end( (addr) ) != begin(HL) jump

                        if_then_else dir<0, <LD A,L>, <LD A,H>
                        if_then_else dir<0, <LD (addr), A>, <LD (addr+1), A>
                        RET
.failed
                    ENDM


_add_min_interval
; L - y_min
; H - y_max
; Preserves BC, DE, IX, IY, ALL'.
                    _draw2D_convex_try_save_interval -1, _xmin_yminmax_part1
                    _draw2D_convex_try_save_interval -1, _xmin_yminmax_part2

                    LD HL,_interval_bad
                    LD (_xmin_yminmax_part1),HL

                    RET


_add_max_interval
; L - y_min
; H - y_max
; Preserves BC, DE, IX, IY, ALL'.
                    _draw2D_convex_try_save_interval +1, _xmax_yminmax_part1
                    _draw2D_convex_try_save_interval +1, _xmax_yminmax_part2

                    LD HL,_interval_bad
                    LD (_xmax_yminmax_part1),HL

                    RET


fill
; HL - pattern_8x8 (8 aligned)
; Preserves IXH, IY.
                    LD (.pattern),HL

                    ; In any case, we have to call 'clear' at the end.
                    LD HL,clear
                    PUSH HL

                    ; Loading  _xmin_yminmax_part1 into BC.
                    LD BC,_interval_empty
.xmin_yminmax_part1 EQU $-2
                    LD A,LOW(_interval_empty)
                    CP C
                    RET Z ; if (_xmin_yminmax_part1 == _interval_empty) return
                    ; Concatenating _xmin_yminmax_part2 to BC.
                    LD DE,_interval_empty
.xmin_yminmax_part2 EQU $-2
                    CP E
                    JP Z,.skip_xmin_yminmax_part2 ; if (_xmin_yminmax_part2 == _interval_empty) jump
                    LD A,B
                    CP E
                    RET NZ ; if (begin(_xmin_yminmax_part1) != end(_xmin_yminmax_part2)) return
                    LD B,D
.skip_xmin_yminmax_part2
                    ; BC = _xmin_yminmax = [begin(_xmin_yminmax_part2), end(_xmin_yminmax_part1)]

                    ; Loading  _xmax_yminmax_part1 into HL.
                    LD HL,_interval_empty
.xmax_yminmax_part1 EQU $-2
                    LD A,HIGH(_interval_empty)
                    CP H
                    RET Z ; if (_xmax_yminmax_part1 == _interval_empty) return
                    ; Concatenating _xmax_yminmax_part2 to HL.
                    LD DE,_interval_empty
.xmax_yminmax_part2 EQU $-2
                    CP D
                    JP Z,.skip_xmax_yminmax_part2 ; if (_xmax_yminmax_part2 == _interval_empty) jump
                    LD A,L
                    CP D
                    RET NZ ; if (end(_xmax_yminmax_part1) != begin(_xmax_yminmax_part2)) return
                    LD L,E
.skip_xmax_yminmax_part2
                    ; HL = _xmax_yminmax = [begin(_xmin_yminmax_part1), end(_xmin_yminmax_part2)]

                    LD A,C
                    CP L
                    JP NC,$+4 ; usually _x_min_y_min == _x_max_y_min and we will jump, so using JP.
                    LD C,L
                    ; C = y_min = max(_x_min_y_min, _x_max_y_min)

                    LD A,B
                    CP H
                    JR C,$+3 ; usually _x_min_y_max == _x_max_y_max and we will not jump, so using JR.
                    LD A,H
                    ; A = y_max = min(_x_min_y_max, _x_max_y_max)

                    SUB C
                    RET C ; if (y_min > y_max) or
                    RET Z ;    (y_min == y_max) return
                    LD B,A
                    ; C = y = y_min
                    ; B = y_max - y_min

                    ; Testing if figure is inverted (_x_max[i] > _x_min[i]).
                    RRA
                    ADD C
                    LD L,A
                    ; L = y_mid = (y_min + y_max) / 2
                    LD H,HIGH(_x_max)
                    LD A,(HL)
                    DEC H
                    CP (HL)
                    RET C ; if (_x_min[y_mid] > _x_max[y_mid]) return
.test1_passed
                    JP NZ,.test2_passed
                    INC L
                    LD A,(HL)
                    INC H
                    CP (HL)
                    RET NC ; if (_x_max[y_mid+1] <= _x_min[y_mid+1]) return
.test2_passed

                    INC B ; B = y_max - y_min + 1
.loop
                    LD A,C
                    EXX
                    LD L,A
                    ; L = y

                    LD H,HIGH(screen_table)
                    LD E,(HL)
                    INC H
                    LD D,(HL)
                    ; DE = line_address[y]

                    LD H,HIGH(_x_min)
                    LD B,(HL)
                    INC H
                    LD C,(HL)
                    ; B = x_min = _x_min[y]
                    ; C = x_max = _x_max[y]

                    LD A,7
                    AND L
                    LD HL,0
.pattern            EQU $-2
                    ADD L
                    LD L,A
                    LD A,(HL)
                    LD IXL,A
                    ; IXL = pattern_8x1 = pattern_8x8[y % 8]

                    LD HL,.l_masks
                    LD A,7
                    AND B
                    ADD L
                    LD L,A

                    LD A,(HL)
                    EX AF,AF'

                    LD HL,.r_masks
                    LD A,7
                    AND C
                    ADD L
                    LD L,A

                    LD L,(HL)
                    EX AF,AF'
                    LD H,A

                    EX DE,HL
                    ; D = l_mask = l_masks[x_min % 8]
                    ; E = r_mask = r_masks[x_max % 8]
                    ; HL = line_address[y]

                    LD A,7
                    AND B
                    XOR B
                    RRA
                    RRA
                    RRA
                    LD B,A
                    ; B = x_min >> 3

                    ADD L
                    LD L,A
                    ; HL = pixel_address = line_address[y] + (x_min >> 3)

                    LD A,7
                    AND C
                    XOR C
                    RRA
                    RRA
                    RRA
                    SUB B
                    ; A = (x_max >> 3) - (x_min >> 3)

                    JR C,.next
                    JR Z,.short
.long
                    LD B,A

                    LD A,D
                    AND IXL
                    LD C,A
                    LD A,D
                    CPL
                    AND (HL)
                    OR C
                    LD (HL),A
                    ; (pixel_address) = l_mask & pattern_8x1 | ~l_mask & (pixel_address)

                    LD A,IXL ; A = pattern_8x1

                    INC L ; pixel_address += 1
                    DEC B
                    JR Z,2F

1                   LD (HL),A ; (pixel_address) = pattern_8x1
                    INC L     ; pixel_address += 1
                    DJNZ 1B

2                   AND E
                    LD C,A
                    LD A,E
                    CPL
                    AND (HL)
                    OR C
                    LD (HL),A
                    ; (pixel_address) = r_mask & pattern_8x1 | ~r_mask & (pixel_address)
.next
                    EXX
                    INC C ; y += 1
                    DJNZ .loop

                    RET
.short
                    LD A,D
                    AND E
                    LD D,A
                    AND IXL
                    LD C,A
                    LD A,D
                    CPL
                    AND (HL)
                    OR C
                    LD (HL),A
                    ; (pixel_address) = (l_mask & r_mask) & pattern_8x1 | ~(l_mask & r_mask) & (pixel_address)

                    EXX
                    INC C ; y += 1
                    DJNZ .loop

                    RET

.l_masks            EQU draw2D.draw_horizontal_line.l_masks
.r_masks            EQU draw2D.draw_horizontal_line.r_masks


clear
; Preserves ALL except HL.
                    LD HL,_interval_empty
                    LD (_xmin_yminmax_part1),HL
                    LD (_xmin_yminmax_part2),HL
                    LD (_xmax_yminmax_part1),HL
                    LD (_xmax_yminmax_part2),HL

                    RET

                    ENDMODULE ; convex


                    ALIGN 8

white               BYTE %11111111
                    BYTE %11111111
                    BYTE %11111111
                    BYTE %11111111
                    BYTE %11111111
                    BYTE %11111111
                    BYTE %11111111
                    BYTE %11111111

black               BYTE %00000000
                    BYTE %00000000
                    BYTE %00000000
                    BYTE %00000000
                    BYTE %00000000
                    BYTE %00000000
                    BYTE %00000000
                    BYTE %00000000

chess               BYTE %10101010
                    BYTE %01010101
                    BYTE %10101010
                    BYTE %01010101
                    BYTE %10101010
                    BYTE %01010101
                    BYTE %10101010
                    BYTE %01010101

chess2              BYTE %01010101
                    BYTE %10101010
                    BYTE %01010101
                    BYTE %10101010
                    BYTE %01010101
                    BYTE %10101010
                    BYTE %01010101
                    BYTE %10101010

                    ENDMODULE
                    milestone
