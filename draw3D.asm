                    MODULE draw3D

; TODO: Projection and other transformation are rudimentary now.


position_x          EQU set_vertices.position_x
position_y          EQU set_vertices.position_y
position_z          EQU set_vertices.position_z

roll                EQU set_vertices.roll
pitch               EQU set_vertices.pitch
yaw                 EQU set_vertices.yaw


rotate
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

                    LD IYL,C ; IYL = index (IXL is used by div_uABC_uDE_ABC)
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

                    CALL .project
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

                    CALL .project
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
.project
; TODO: Review.
; TODO: screen distance should be variable.
; TOOD: round up division result?
; HL - x_or_y
; DE - z
; SF - sign(x_or_y)
; Output:
; HL = screen_x_or_y  = x_or_y * 256 / (z + 256)
; Preserves IXH, IY, BC', DE', HL'.
                    JP M,.lt_0
.ge_0
                    LD A,H
                    LD B,L
                    LD C,0
                    ; ABC = x_or_y

                    INC D ; DE = z + 256

                    CALL div_uABC_15bitDE_ABC

                    LD H,B
                    LD L,C
                    ; HL = screen_x_or_y = ABC / DE

                    RET
.lt_0
                    XOR A
                    LD C,A
                    SUB L
                    LD B,A
                    SBC A
                    SUB H
                    ; ABC = -x_or_y

                    INC D ; DE = z + 256

                    CALL div_uABC_15bitDE_ABC

                    XOR A
                    SUB C
                    LD L,A
                    SBC A
                    SUB B
                    LD H,A
                    ; HL = screen_x_or_y = -(ABC / DE)

                    RET


                    MACRO _draw3D_load_point
                    ; L - index
                    ; Output:
                    ; BC - _vertex_screen_x[index].x
                    ; DE - _vertex_screen_y[index].y
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


draw_point
; Draws single point using 'index' vertex.
; L - index
; Preserves IX, IY, ALL'.
                    _draw3D_load_point
                    JP draw2DEX.draw_point


draw_line
; Draws line between 'index1' and 'index2' vertices.
; H - index1
; L - index2
                    LD A,H
                    _draw3D_load_point
                    EXX
                    LD L,A
                    _draw3D_load_point

                    JP draw2DEX.draw_line

                    ENDMODULE
/* TODO
model:
; HL - model address
; format: {
;   vertex_count: byte
;   vertices[vertex_count]: {
;     x: byte
;     y: byte
;     z: byte
;   }
;   face_count: byte
;   faces[face_count]: {
;     normal: {
;       x: byte
;       y: byte
;       z: byte
;     }
;     pattern_addr: word
;     indices: {
;       index1: byte
;       ...
;       indexN: byte
;       -1: byte
;     }
;   }
; }
; Output:
; HL+=size(model)
    LD B,(HL) ; vertex_count
    INC HL
    CALL .process_vertices

    LD B,(HL) ; faces_count
    INC HL

1   PUSH BC
    CALL .draw_face
    POP BC
    DJNZ 1B

    RET

.draw_face:
; HL - face address
; Output:
; HL += sizeof(face)
    CALL _rotate_vector_z_only
    LD A,IXH
    OR A
    JP M,.skip ; if (rotated(normal).z<0) jump

    LD E,(HL)
    INC HL
    LD D,(HL) ; DE=pattern_addr
    INC HL
 
    PUSH DE ; 1->

    LD BC,_polygon
    LD D,HIGH(_projected)
1:
    LD E,(HL) ; index
    INC HL

    LD A,E
    INC A
    JR Z,2F ; if (index==-1) break

    LD A,(DE) ; A=_projected[index].x
    LD (BC),A ; _polygon[i]=A
    INC D
    INC C ; i+=1

    LD A,(DE) ; A=_projected[index].y
    LD (BC),A ; _polygon[i]=A
    DEC D
    INC C ; i+=1

    JP 1B
2:
    POP DE; 1<-

    LD B,C
    SRL B ; points count

    PUSH HL
    LD HL,_polygon
    CALL draw2D.polygon_fill_8x8
    POP HL

    RET
.skip:
    INC HL
    INC HL

    LD A,-1
    LD BC,0
    CPIR
    RET


_rotate_vector_z_only:
; Same as _rotate_vector but calculates only z coordinate.
; HL - [x,y,z]
; Output:
; let [x2,y2,z2]=yaw(pitch(roll([x,y,z])))
; IXH=z2
; HL+=3
; Not modified:
; BC
    LD A,(HL)
    INC HL
    LD IXL,A ; x

    LD A,(HL)
    INC HL
    LD IYL,A ; y

    LD E,(HL) ; z
    INC HL

    LD A,(roll)
    EXX
    CALL rotate
    EXX

    LD D,IXH ; x'
    LD IXL,E ; z'
    LD IYL,IYH ; y'

    LD A,(pitch)
    NEG
    EXX
    CALL rotate_x_only
    EXX

    LD IXL,IXH ; z''
    LD IYL,D ; x''

    LD A,(yaw)
    EXX
    CALL rotate_x_only
    EXX

    RET
*/
