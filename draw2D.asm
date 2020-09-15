                    MODULE draw2D

draw_point
; DE - x, y
; Preserves IX, IY, EX.
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
                    ; Input:
                    ; H = HIGH(screen_table)
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
                    ; Input:
                    ; DE - pixel_address
                    ; C - mask
                    ; Output:
                    ; (pixel_address) |= mask
                        LD A,(DE)
                        OR C
                        LD (DE),A
                    ENDM

                    MACRO _draw2D_draw_line_init
                    ; Input:
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

                    MACRO _draw2D_draw_line_by_x shift_op, mask_op
                    ; shift_op - INC/DEC depending on sign(x2-x1)
                    ; mask_op  - RRC/RLC depending on sign(x2-x1)
                    ; Input:
                    ; H - dx = abs(x2 - x1)
                    ; L - dy = y2 - y1
                    ; D - x = x1
                    ; E - y = y1
                    ; Special: Makes RET!
                        _draw2D_draw_line_init
                        LD A,H
                        RRA ; A = dx / 2
                        LD B,H ; B = counter = dx
.loop                   ADD L ; A += dy
                        JR C,.step_y ; if (A > 256) jump
                        CP H
                        JR NC,.step_y ; if (A >= dx) jump
                        EXX
                            EX AF,AF'
.step_x                     mask_op C ; RRC/RLC mask
                            JP NC,1F
                            shift_op B ; shift +/-= 1
                            shift_op E ; pixel_address +/-=1
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

                    MACRO _draw2D_draw_line_by_y shift_op, mask_op
                    ; shift_op - INC/DEC depending on sign(x2-x1)
                    ; mask_op  - RRC/RLC depending on sign(x2-x1)
                    ; Input:
                    ; H - dx = abs(x2 - x1)
                    ; L - dy = y2 - y1
                    ; D - x = x1
                    ; E - y = y1
                    ; Special:
                    ; Makes RET!
                        _draw2D_draw_line_init
                        LD A,L
                        RRA ; A = dy / 2
                        LD B,L ; B = counter = dy
.loop                   ADD H ; A += dx
                        JR C,.step_x ; if (A > 256) jump
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
                            mask_op C ; RRC/RLC mask
                            JP NC,.step_y
                            shift_op B ; shift +/-= 1
                            JP .step_y
                    ENDM
.left_to_right
                    LD H,A
                    LD L,C
                    ; H = dx = x2 - x1
                    ; L = dy = y2 - y1

                    CCF
                    SBC C
                    JP C,.left_to_right_by_y
.left_to_right_by_x _draw2D_draw_line_by_x INC, RRC
.left_to_right_by_y _draw2D_draw_line_by_y INC, RRC

.right_to_left
                    CPL
                    INC A
                    LD H,A
                    LD L,C
                    ; H = dx = x1 - x2
                    ; L = dy = y2 - y1

                    SBC C
                    JP C,.right_to_left_by_y
.right_to_left_by_x _draw2D_draw_line_by_x DEC, RLC
.right_to_left_by_y _draw2D_draw_line_by_y DEC, RLC


draw_horizontal_line
; D - x1
; H - x2
; E - y
; Preserves IX, IY, EX.
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
; Preserves IX, IY, EX.
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

                    ENDMODULE
/*
_horz_line:
; DE - x2, x1
; L  - y
; H  - pattern
    LD A,D
    CP E
    RET C

    PUSH HL ; store pattern

    LD H,HIGH(screen_table)
    LD A,(HL)
    INC H
    LD H,(HL)
    LD L,A ; HL=line_base_addr=screen_table[y]

    ; hack
    LD A,H
    ADD HIGH(screen_buffer-#4000)
    LD H,A
    ; hack

    PUSH HL ; store line_base_addr

    LD HL,l_bits_mask
    LD C,E
    SRL E
    SRL E
    SRL E ; E = byte1 = x1/8
    LD A,7
    AND C
    ADD A,L
    LD L,A
    LD C,(HL) ; C = mask1 = l_bits_mask[x1%8]

    LD L,LOW(l_bits_mask)
    LD B,D
    SRL D
    SRL D
    SRL D ; D = byte2 = x2/8
    LD A,7
    AND B
    INC A
    ADD A,L
    LD L,A
    LD A,(HL)
    CPL
    LD B,A ; B = mask2 = ~l_bits_mask[x2%8+1]

    POP HL ; HL = line_base_addr
    LD A,D
    LD D,0
    ADD HL,DE ; HL += byte1
    SUB E ; A = byte2 - byte1
    JR Z,.single_byte_line ; if (byte2 == byte1) jump

    MACRO draw_byte
    ; HL - byte_addr
    ; C - mask
    ; D - pattern
        LD A,(HL)
        AND C
        LD E,A ; E = old = *byte_addr & mask

        LD A,C
        CPL
        AND D ; A = new = pattern & ~mask

        OR E
        LD (HL),A ; *byte_addr = old | new = (*byte_addr & mask) | (pattern & ~mask)
    ENDM

.multi_byte_line:
    LD D,A
    LD E,B
    POP AF
    PUSH DE ; store byte2 - byte1, mask2
    LD D,A ; D = pattern

    draw_byte ; drawing first byte

    LD A,D
    POP DE
    LD B,D ; B = byte2 - byte1
    LD C,E ; C = mask2
    LD D,A

    INC HL
    DEC B
    JR Z,.last ; if (byte2 - byte1 == 1) jump
.next:
    LD (HL),D ; drawing intermediate byte
    INC HL
    DJNZ .next
.last:
    draw_byte ; drawing last byte
    RET

.single_byte_line:
    POP DE ; D = pattern
    LD A,B
    OR C
    LD C,A ; C = mask1 | mask2

    draw_byte ; drawing single byte
    RET


polygon_fill:
; B  - count
; HL - coords (array [x1,y1,x2,y2...xn,yn] clockwise)
; Not modified:
; IX,IY,EX
    CALL _polygon_write
    RET C ; nothing to do
1:
    LD L,C ; y

    LD H,HIGH(_x_min)
    LD E,(HL) ; _x_min[y]
    INC H
    LD D,(HL) ; _x_max[y]

    LD H,255 ; solid pattern

    PUSH BC
    CALL _horz_line
    POP BC

    INC C
    DJNZ 1B

    RET


polygon_fill_8x8:
; B  - count
; HL - coords (array [x1,y1,x2,y2...xn,yn] clockwise)
; DE - pattern (8 bytes array from top to bottom, 8 aligned)
; Not modified:
; IX,IY,EX
    PUSH DE
    CALL _polygon_write
    POP DE
    RET C ; nothing to do
1:
    LD A,C
    CPL
    AND 7
    OR E
    PUSH DE
    LD E,A
    LD A,(DE) ; pattern[y%8]

    LD L,C ; y
    PUSH BC

    LD H,HIGH(_x_min)
    LD E,(HL) ; _x_min[y]
    INC H
    LD D,(HL) ; _x_max[y]

    LD H,A ; pattern

    CALL _horz_line
    POP BC
    POP DE

    INC C
    DJNZ 1B

    RET


_polygon_write:
; B  - count
; HL - coords (array [x1,y1,x2,y2...xn,yn] clockwise)
; Output:
; CF = is_invisible (flat or inverted polygon)
; B = max(y)-min(y)+1
; C = min(y)
    LD A,B ; count

    LD D,(HL) ; x[n]
    INC HL
    LD E,(HL) ; y[n]
    INC HL
    PUSH DE ; 1->

    LD B,E ; y_min = y1
    LD C,E ; y_max = y1

    DEC A
    JR Z,.last
.next:
    PUSH AF ; 2->

    PUSH BC ; 3->
    LD B,(HL) ; x[n+1]
    INC HL
    LD C,(HL) ; y[n+1]
    INC HL

    PUSH HL ; 4->
    PUSH BC ; 5->
    LD H,B
    LD L,C
    CALL _write_polygon_line
    POP DE ; 5<-
    POP HL ; 4<-

    POP BC ; 3<-
    LD A,E
    minmax_step C,B

    POP AF ; 2<-
    DEC A
    JP NZ,.next
.last:
    POP HL ; 1<-

    PUSH BC
    CALL _write_polygon_line
    POP BC ; y_max,y_min

    LD A,B
    INC C
    JR Z,.return_error ; if (y_min==255) jump
    SUB C ; is_invisible=(y_min==y_max)
    RET C ; if (is_invisible) return
    DEC C
    INC A
    INC A
    LD B,A ; y_max-y_min+1

    RRA
    ADD C ; A=y_mid=(y_min+y_max)/2
    LD H,HIGH(_x_min)
    LD L,A
    LD D,(HL); _x_min[y_mid]
    INC H
    LD A,(HL); _x_max[y_mid]
    CP D ; if (_x_min[y_mid]>_x_max[y_mid]) is_invisible=true
    RET NZ ; if (_x_min[y_mid]!=_x_max[y_mid]) return

    INC L
    LD A,(HL); _x_max[y_mid+1]
    DEC H
    LD D,(HL); _x_min[y_mid+1]
    INC D
    JR Z,.return_error ; if (_x_min[y_mid+1]==255) jump
    CP D ; if (_x_min[y_mid+1]>=_x_max[y_mid+1]) is_invisible=true

    RET
.return_error:
    OR A
    CCF ; is_invisible=true
    RET
    
    
_write_polygon_line:
; DE - x1,y1
; HL - x2,y2
    LD A,E
    SUB L
    JP Z,_write_polygon_line_dy_eq_0 ; if (y1==y2) jump
    ; def xx1=y1<y2?x1:x2, yy1=min(y1,y2), xx2=y1<y2?x2:x1, yy2=max(y1,y2)
    JR C,2F ; if (y1<y2) jump
1:
    LD C,H
    LD H,HIGH(_x_max)
    JP 3F
2:
    NEG
    EX DE,HL
    LD C,H
    LD H,HIGH(_x_min)
3:
    ; now C=xx1, L=yy1, D=xx2, E=yy2, H=high(y1<y2?_x_min:_x_max), A=yy2-yy1
    LD E,A
    LD A,C
    SUB D
    JR C,2F ; if (xx2>xx1) jump
1:
    LD D,A
    LD B,#0D ; DEC C
    JP 3F
2:
    NEG
    LD D,A
    LD B,#0C ; INC C
3:
    ; now C=x=xx1, L=y=yy1, D=abs(xx2-xx1), E=yy2-yy1, H=high(y1<y2?_x_min:_x_max), B=opcode(xx1<xx2?'INC C':'DEC C')
    LD A,E
    CP D
    ; let w=abs(xx2-xx1), l=yy2-yy1
    JP C,_write_polygon_line_dy_lt_dx ; if (l<w) jump
    ; if (l>=w) fall through _write_polygon_line_dy_ge_dx


_write_polygon_line_dy_ge_dx
; C - x=x1
; L - y=y1
; D - w=abs(x2-x1)
; E - l=y2-y1
; H - high(y1<y2?_x_min:_x_max)
; B - opcode(x1<x2?'INC C':'DEC C')
    LD A,B
    LD (.placeholder),A
    LD B,E ; B = l
    LD A,E
    RRA ; A=l/2
    LD (HL),C ; write first point
1:
    INC L ; y+=1
    ADD D ; A+=w
    JR C,3F
    CP E
    JR NC,3F ; if (A>l) jump
2:
    LD (HL),C ; write point
    DJNZ 1B
    RET
3:
    SUB E ; A-=l
.placeholder:
    NOP ; INC C/DEC C - x+=1/x-=1
    JP 2B


_write_polygon_line_dy_lt_dx
; C - x=x1
; L - y=y1
; D - w=abs(x2-x1)
; E - l=y2-y1
; H - high(y1<y2?_x_min:_x_max)
; B - opcode(x1<x2?'INC C':'DEC C')
    LD A,#0C ; INC C
    CP B
    LD A,HIGH(_x_min)
    JR Z,2F ; if (xx1<xx2) jump
1:
    CP H
    JP Z,_write_polygon_line_dy_lt_dx_2 ; if (y1<y2) jump
    JP _write_polygon_line_dy_lt_dx_1
2:
    CP H
    JP Z,_write_polygon_line_dy_lt_dx_1 ; if (y1<y2) jump
    JP _write_polygon_line_dy_lt_dx_2


_write_polygon_line_dy_lt_dx_1
; C - x=x1
; L - y=y1
; D - w=abs(x2-x1)
; E - l=y2-y1
; H - high(y1<y2?_x_min:_x_max)
; B - opcode(x1<x2?'INC C':'DEC C')
    LD A,B
    LD (.placeholder),A
    LD B,D ; B = w
    LD A,D
    SRL A ; A=w/2
    LD (HL),C ; write first point
1:
.placeholder:
    NOP ; INC C/DEC C - x+=1/x-=1
    ADD E ; A+=l
    JR C,3F
    CP D
    JR NC,3F ; if (A>w) jump
2:
    DJNZ 1B
    RET
3:
    SUB D ; A-=w
    INC L ; y+=1
    LD (HL),C ; write point
    DJNZ 1B
    RET


_write_polygon_line_dy_lt_dx_2
; C - x=x1
; L - y=y1
; D - w=abs(x2-x1)
; E - l=y2-y1
; H - high(y1<y2?_x_min:_x_max)
; B - opcode(x1<x2?'INC C':'DEC C')
    LD A,B
    LD (.placeholder),A
    LD B,D ; B = w
    LD A,D
    SRL A ; A=w/2
1:
    ADD E ; A+=l
    JR C,3F
    CP D
    JR NC,3F ; if (A>w) jump
2:
.placeholder:
    NOP ; INC C/DEC C - x+=1/x-=1
    DJNZ 1B
    LD (HL),C ; write last point
    RET
3:
    SUB D ; A-=w
    LD (HL),C ; write point
    INC L ; y+=1
    JP 2B


_write_polygon_line_dy_eq_0
; DE - x1,y
; HL - x2,y
    LD A,H
    CP D
    LD H,HIGH(_x_min)
    JR C,2F ; if (x1>x2) jump
1:
    LD (HL),D ; write left point
    INC H
    LD (HL),A ; write right point
    RET
2:
    LD (HL),A ; write left point
    INC H
    LD (HL),D ; write right point
    RET
*/
