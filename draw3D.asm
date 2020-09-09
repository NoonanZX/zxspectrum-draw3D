/*
    MODULE draw3D

roll:
    BYTE 0
pitch:
    BYTE 0
yaw:
    BYTE 0


rotate:
; A - angle (clock-wise)
; IXL - x
; IYL - y
; Output:
; IXH = +x*cos(angle)+y*sin(angle)
; IYH = -x*sin(angle)+y*cos(angle)
; Not modified:
; EX,IXL,IYL
    LD IYH,A ; angle

    LD B,IXL ; x
    LD H,HIGH(cos_table)
    LD L,A
    LD C,(HL) ; cos(angle)
    CALL fmul_BxC_A
    LD IXH,A ; x*cos(angle)

    LD B,IYL ; y
    LD D,HIGH(sin_table)
    LD E,IYH
    EX DE,HL
    LD C,(HL) ; sin(angle)
    CALL fmul_BxC_A
    ADD IXH
    LD IXH,A ; y*sin(angle)+x*cos(angle)

    LD B,IXL ; x
    LD D,HIGH(sin_table)
    LD E,IYH
    EX DE,HL
    LD C,(HL) ; sin(angle)
    CALL fmul_BxC_A
    LD E,IYH ; angle
    LD IYH,A ; x*sin(angle)

    LD B,IYL ; y
    LD D,HIGH(cos_table)
    EX DE,HL
    LD C,(HL) ; cos(angle)
    CALL fmul_BxC_A
    SUB IYH ; y*cos(angle)-x*sin(angle)
    LD IYH,A

    RET


rotate_x_only:
; A - angle (clock-wise)
; IXL - x
; IYL - y
; Output:
; IXH = +x*cos(angle)+y*sin(angle)
; Not modified:
; EX,IXL,IYL
    LD IYH,A ; angle

    LD B,IXL ; x
    LD H,HIGH(cos_table)
    LD L,A
    LD C,(HL) ; cos(angle)
    CALL fmul_BxC_A
    LD IXH,A ; x*cos(angle)

    LD B,IYL ; y
    LD D,HIGH(sin_table)
    LD E,IYH
    EX DE,HL
    LD C,(HL) ; sin(angle)
    CALL fmul_BxC_A
    ADD IXH
    LD IXH,A ; y*sin(angle)+x*cos(angle)

    RET


rotate_y_only:
; A - angle (clock-wise)
; IXL - x
; IYL - y
; Output:
; IYH = -x*sin(angle)+y*cos(angle)
; Not modified:
; EX,IXL,IYL
    LD IXH,A ; angle

    LD B,IXL ; x
    LD H,HIGH(sin_table)
    LD L,A
    LD C,(HL) ; sin(angle)
    CALL fmul_BxC_A
    LD IYH,A ; x*sin(angle)

    LD B,IYL ; y
    LD D,HIGH(cos_table)
    LD E,IXH ; angle
    EX DE,HL
    LD C,(HL) ; cos(angle)
    CALL fmul_BxC_A
    SUB IYH ; y*cos(angle)-x*sin(angle)
    LD IYH,A

    RET


project:
; Projecting coord on screen (screen_distance=256).
; A - x_or_y
; IXH - z
; Output:
; A=x_or_y*screen_distance/(screen_distance-z)
    OR A
    JP M,.negative ; if (x_or_y<0) jump
.positive:
    LD B,A
    XOR A
    LD C,A ; BC=x_or_y*256

    LD D,A
    SUB IXH
    JP M,$+4
    INC D
    LD E,A ; DE=256-z

    CALL div_BCxDE_BC

    LD A,C ; A=x_or_y*256/(256-z)

    RET
.negative:
    NEG
    LD B,A
    XOR A
    LD C,A ; BC=-x_or_y*256

    LD D,A
    SUB IXH
    JP M,$+4
    INC D
    LD E,A ; DE=256-z

    CALL div_BCxDE_BC

    LD A,C ; A=-x_or_y*256/(256-z)
    NEG

    RET


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

.process_vertices:
; B - count
; HL - buffer
; Output:
; HL+=3*count
    LD C,0 ; i
1:
    CALL _rotate_vector

    LD A,E ; y
    LD E,C ; i
    EXX
    CALL project
    EXX
    LD C,A ; y

    LD A,D ; x
    LD D,HIGH(_projected)
    EXX
    CALL project
    EXX

    ADD 128
    LD (DE),A ; x

    INC D
    LD A,C
    ADD 96
    LD (DE),A ; y

    LD C,E ; i
    INC C ; i+=1

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


_rotate_vector:
; Reads 3-byte vector from (HL) and consequently rotates it around OZ,OX,OY using angles (roll),(pitch),(yaw).
; HL - [x,y,z]
; Output:
; let [x2,y2,z2]=yaw(pitch(roll([x,y,z])))
; D=x2
; E=y2
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
    CALL rotate
    EXX

    LD E,IYH ; y''
    LD IXL,IXH ; z''
    LD IYL,D ; x''

    LD A,(yaw)
    EXX
    CALL rotate
    EXX

    LD D,IYH ; x2

    RET


_rotate_vector_z_only:
; Save as _rotate_vector but calculates only z coordinate.
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

    ENDMODULE
*/
