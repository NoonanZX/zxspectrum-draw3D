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
                    JP Z,draw_vertical_line_nocheck

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
                    ; Special - makes RET!
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
                    ; Special - makes RET!
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
                    JP C,draw_horizontal_line_nocheck
                    JP Z,draw_point
                    EX DE,HL
                    LD E,L
draw_horizontal_line_nocheck
; Same as above, but requires x1 <= x2.
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
                    JP C,draw_vertical_line_nocheck
                    JP Z,draw_point
                    EX DE,HL
                    LD D,H
draw_vertical_line_nocheck
; Same as above, but requires y1 <= y2.
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
; Stack - top => (x1, y1) ... (xn, yn), -1 - Points ordered counterclockwise. Low byte - x, high byte - y. Value -1 is end mark.
; HL - pattern_8x8
; Preserves DE, IXH, IY.
                    POP BC
                    EXX

                    POP DE ; x1, y1
                    LD (.first_point),DE

                    POP HL ; x2, y2
                    CALL convex.add_line

                    POP HL ; x3, y3
.loop               CALL convex.add_line

                    POP HL ; xn, yn
                    LD A,L
                    INC A
                    JP NZ,.loop ; if (yn <> -1) jump

                    LD HL,0 ; x1, y1
.first_point        EQU $-2
                    CALL convex.add_line

                    EXX
                    PUSH BC
                    JP convex.fill


                    MODULE convex
                    ; To draw convex figure - define border with add_xxx routines, then call fill.
                    ; Note that border must be defined clockwise, opposed to draw_polygon in which points are counterclockwise ordered.

add_horizontal_line
; DE - x1, y
; HL - x2, y
; E = L
; Output:
; DE = x2, y
; Preserves E, L, IX, IY, ALL'.
                    LD A,H
                    CP D
                    RET Z
                    LD D,H
                    JP C,.x1_gt_x2
.x1_lt_x2
                    LD H,HIGH(_x_max)
                    LD (HL),A
                    RET
.x1_gt_x2
                    LD H,HIGH(_x_min)
                    LD (HL),A
                    LD H,L
                    JP _add_interval


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

1                   LD (HL),D
                    INC L
                    DJNZ 1B
                    LD (HL),D

                    RET
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
                    JP _add_interval


add_line
; DE - x1, y1
; HL - x2, y2
; Output:
; DE - x2, y2
; Preserves IX, IY, BC', DE', HL'.
                    LD A,L
                    SUB E
                    JP Z,add_horizontal_line

                    EX AF,AF'

                    LD A,H
                    SUB D
                    JP Z,add_vertical_line

                    MACRO _draw2D_convex_add_line dir_x, dir_y
                    ; dir_x = sign(x2 - x1)
                    ; dir_y = sign(y2 - y1)
                    ; DE - x1, y1
                    ; H - dx = abs(x1 - x2)
                    ; L - dy = abs(y1 - y2)
                    ; A = L
                    ; Output:
                    ; D = x1 + dx
                    ; E = y1 + dy
                    ; Preserves IX, IY, ALL'.
                    ; Special - makes RET!
                        CP H

                        EX DE,HL ; (D, E) = (dx, dy)
                        LD C,H   ; (C, L) = (x, y)

                        IF dir_y > 0
                        LD H,HIGH(_x_max)
                        ELSE
                        LD H,HIGH(_x_min)
                        ENDIF

                        JP C,.by_x
.by_y
                        LD B,A ; counter = dy
                        RRA ; A = dy / 2
                        LD (HL),C
1                       ADD D ; A += dx
                        JR C,2F ; if (A >= 256) jump
                        CP E
                        JR C,3F ; if (A < dy) jump
2                       SUB E ; A -= dy
                        if_then_else dir_x>0, INC C, DEC D ; x +/-= 1
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
                        IF dir_y < 0
                            LD A,L
                            ADD E
                            LD H,A
                        ENDIF

                        LD D,C
                        LD E,L

                        IF dir_y < 0
                            JP _add_interval
                        ELSE
                            RET
                        ENDIF
                    ENDM

                    JP C,.x1_gt_x2
.x1_lt_x2           LD H,A
                    EX AF,AF'
                    JP C,.x1_lt_x2_y1_gt_y2
.x1_lt_x2_y1_lt_y2  LD L,A
                    _draw2D_convex_add_line +1, +1
.x1_lt_x2_y1_gt_y2  NEG
                    LD L,A
                    _draw2D_convex_add_line +1, -1
.x1_gt_x2           NEG
                    LD H,A
                    EX AF,AF'
                    JP C,.x1_gt_x2_y1_gt_y2
.x1_gt_x2_y1_lt_y2  LD L,A
                    _draw2D_convex_add_line -1, +1
.x1_gt_x2_y1_gt_y2  NEG
                    LD L,A
                    _draw2D_convex_add_line -1, -1


fill
; HL - pattern_8x8 (8 aligned)
; Preserves DE, IXH, IY.
                    LD (.pattern),HL

                    ; todo - checks

                    LD BC,(_y_minmax)
                    LD A,B
                    SUB C
                    RET C
                    LD B,A
                    ; C = y = _y_min
                    ; B = _y_max - _y_min

                    ; Testing if figure is inverted (_x_max[i] > _x_min[i]).
                    RRA
                    ADD C
                    LD L,A
                    ; L = y_mid = (_y_min + _y_max) / 2

                    LD H,HIGH(_x_min)
                    LD A,(HL)
                    INC H
                    CP (HL)
                    JP C,.ok ; if (_x_min[y_mid] < _x_max[y_mid]) test = ok
                    JP NZ,clear ; if (_x_min[y_mid] > _x_max[y_mid]) test = fail

                    XOR A
                    CP B
                    JP Z,.ok ; if (_x_min[y_mid] == _x_max[y_mid] && _y_min == _y_max) test = ok

                    INC L
                    LD A,(HL)
                    DEC H
                    CP (HL)
                    JP C,clear ; if (_x_min[y_mid + 1] > _x_max[y_mid + 1]) test = fail
.ok                 ; test = ok

                    INC B ; B = _y_max - _y_min + 1
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

                    JP clear
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

                    ; fall through to clear

.l_masks            EQU draw2D.draw_horizontal_line_nocheck.l_masks
.r_masks            EQU draw2D.draw_horizontal_line_nocheck.r_masks


clear
; Preserves ALL except HL.
                    LD HL,#00FF
                    LD (_y_minmax),HL
                    RET


_add_interval
; L - y_min
; H - y_max
; Preserves DE, HL, IX, IY, ALL'.
                    LD BC,#00FF
.y_minmax           EQU $-2

                    LD A,B
                    CP H
                    JR C,1F ; if (y_max > _y_max) jump

                    LD A,L
                    CP C
                    RET NC ; if (y_min >= _y_min) return

                    ; update only _y_min
                    LD (.y_minmax),A
                    RET
1
                    LD A,L
                    CP C
                    JR C,2F ; if (y_min < _y_min) jump

                    ; update only _y_max
                    LD A,H
                    LD (.y_minmax+1),A
                    RET
2
                    ; update both _y_min and _y_max
                    LD (.y_minmax),HL
                    RET


_y_minmax           EQU _add_interval.y_minmax

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
