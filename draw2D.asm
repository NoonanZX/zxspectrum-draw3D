                    MODULE draw2D

draw_point
; DE - x, y
                    LD H,HIGH(screen_table)
                    LD L,E
                    LD C,(HL)
                    INC H
                    LD B,(HL)
                    ; BC = line_address

                    LD A,7
                    AND D
                    LD E,A ; store E = x & 7

                    LD HL,.masks
                    ADD L
                    LD L,A
                    ; HL = mask_address

                    LD A,D
                    XOR E
                    RRA
                    RRA
                    RRA
                    ADD C
                    LD C,A
                    ; BC += (x >> 3)

                    LD A,(BC)
                    OR (HL)
                    LD (BC),A

                    RET

                    ALIGN 8
.masks              BYTE %10000000, %01000000, %00100000, %00010000, %00001000, %00000100, %00000010, %00000001

                    ENDMODULE
/*
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


polygon_fill_1x8:
; B  - count
; D  - pattern
; E  - mode (<0 => RLCA, >0 => RRCA, ==0 => NOP)
; HL - coords (array [x1,y1,x2,y2...xn,yn] clockwise)
; Not modified:
; IX,IY,EX
    PUSH DE
    CALL _polygon_write
    POP DE
    RET C ; nothing to do

    XOR A
    CP E ; mode
    JP Z,3F ; if (mode==0) jump (and A=0=opcode(NOP))
    JP M,2F ; if (mode>0) jump
1:
    LD A,C
    CPL
    AND 7
    LD (.times_1),A
    LD A,D
    JR $
.times_1 = $-1
    RLCA
    RLCA
    RLCA
    RLCA
    RLCA
    RLCA
    RLCA
    LD D,A
    
    LD A,#07 ; RLCA
    JP 3F
2:
    LD A,C
    CPL
    AND 7
    LD (.times_2),A
    LD A,D
    JR $
.times_2 = $-1
    RRCA
    RRCA
    RRCA
    RRCA
    RRCA
    RRCA
    RRCA
    LD D,A

    LD A,#0F ; RRCA
3:
    LD (.op),A

    LD A,D ; pattern
1:
    LD L,C ; y

    LD H,HIGH(_x_min)
    LD E,(HL) ; _x_min[y]
    INC H
    LD D,(HL) ; _x_max[y]

    LD H,A ; pattern

    PUSH AF
    PUSH BC
    CALL _horz_line
    POP BC
    POP AF

    NOP
.op = $-1

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
*/
