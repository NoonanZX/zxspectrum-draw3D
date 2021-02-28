                    milestone
                    MODULE demo

; 1.33.50


viewport_x_min      EQU 0
viewport_y_min      EQU 0
viewport_x_max      EQU 255
viewport_y_max      EQU 191
viewport_x_center   EQU 128
viewport_y_center   EQU 96

x_pos               EQU process_vertex.x_pos
y_pos               EQU process_vertex.y_pos
z_pos               EQU process_vertex.z_pos
roll                EQU process_vertex.roll
pitch               EQU process_vertex.pitch
yaw                 EQU process_vertex.yaw


                    MACRO set_vertex index, x_local, y_local, z_local
                        LD IXL,x_local
                        LD IYL,y_local
                        LD A,z_local

                        CALL process_vertex

                        LD (buffer + 4 * index),HL
                        EXX
                        LD (buffer + 4 * index + 2),HL
                    ENDM


                    MACRO push_vertex index
                        LD HL,(buffer + 4 * index)
                        PUSH HL
                        LD HL,(buffer + 4 * index + 2)
                        PUSH HL
                    ENDM


init
                    LD BC,(viewport_x_center<<8) + viewport_y_center
                    LD DE,(viewport_x_min<<8) + viewport_y_min
                    LD HL,(viewport_x_max<<8) + viewport_y_max
                    JP draw3D.set_viewport


draw
.x_size             EQU 100
.y_size             EQU 100
.z_size             EQU 100

                    set_vertex 0, -.x_size/2, -.y_size/2, +.z_size/2
                    set_vertex 1, +.x_size/2, -.y_size/2, +.z_size/2
                    set_vertex 2, -.x_size/2, +.y_size/2, +.z_size/2
                    set_vertex 3, +.x_size/2, +.y_size/2, +.z_size/2
                    set_vertex 4, -.x_size/2, -.y_size/2, -.z_size/2
                    set_vertex 5, +.x_size/2, -.y_size/2, -.z_size/2
                    set_vertex 6, -.x_size/2, +.y_size/2, -.z_size/2
                    set_vertex 7, +.x_size/2, +.y_size/2, -.z_size/2

                    ; front
                    LD HL,#8000 : PUSH HL
                    push_vertex 4
                    push_vertex 5
                    push_vertex 7
                    push_vertex 6
                    LD HL,draw2D.white
                    CALL draw2DEX.draw_polygon

                    ; back
                    LD HL,#8000 : PUSH HL
                    push_vertex 0
                    push_vertex 2
                    push_vertex 3
                    push_vertex 1
                    LD HL,draw2D.white
                    CALL draw2DEX.draw_polygon

                    ; top
                    LD HL,#8000 : PUSH HL
                    push_vertex 6
                    push_vertex 7
                    push_vertex 3
                    push_vertex 2
                    LD HL,chess_wide
                    CALL draw2DEX.draw_polygon

                    ; bottom
                    LD HL,#8000 : PUSH HL
                    push_vertex 4
                    push_vertex 0
                    push_vertex 1
                    push_vertex 5
                    LD HL,rabica
                    CALL draw2DEX.draw_polygon

                    ; left
                    LD HL,#8000 : PUSH HL
                    push_vertex 0
                    push_vertex 4
                    push_vertex 6
                    push_vertex 2
                    LD HL,draw2D.chess
                    CALL draw2DEX.draw_polygon

                    ; right
                    LD HL,#8000 : PUSH HL
                    push_vertex 5
                    push_vertex 1
                    push_vertex 3
                    push_vertex 7
                    LD HL,draw2D.chess
                    CALL draw2DEX.draw_polygon

                    LD HL,roll
                    DEC (HL)
                    LD HL,pitch
                    DEC (HL)
                    LD HL,yaw
                    DEC (HL)

                    RET


draw_viewport
                    LD D,viewport_x_min
                    LD E,viewport_y_min+1
                    LD H,viewport_x_max
                    LD L,E
                    CALL draw2D.draw_horizontal_line

                    LD D,viewport_x_min
                    LD E,viewport_y_max-1
                    LD H,viewport_x_max
                    LD L,E
                    CALL draw2D.draw_horizontal_line

                    LD D,viewport_x_min+1
                    LD E,viewport_y_min
                    LD H,D
                    LD L,viewport_y_max
                    CALL draw2D.draw_vertical_line

                    LD D,viewport_x_max-1
                    LD E,viewport_y_min
                    LD H,D
                    LD L,viewport_y_max
                    CALL draw2D.draw_vertical_line

                    RET


rotate
; A - angle (counter-clock-wise)
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
                    ADD 128 ; angle + pi
                    LD B,IYL ; y
                    CALL mul_sinA_sB_HL                 

                    POP DE
                    ADD HL,DE
                    LD A,H
                    LD IXH,A ; IXH = +x*cos(angle) + y*sin(angle)

                    LD A,IYH ; angle
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


process_vertex
; [IXL, IYL, A] - [x, y, z]
; Output:
; [HL, HL'] = project([x_pos, y_pos, z_pos] + rotate(rotate(rotate([x, y, z], OZ, roll), OX, pitch), OY, yaw))
; Preserves NOTHING.
                    EX AF,AF'
                    LD A,0
.roll               EQU $-1
                    CALL rotate
                    ; [IXH, IYH, A'] = rotate([x, y, z], OZ, roll)

                    LD A,IXH
                    EX AF,AF'
                    LD IXL,A
                    LD IYL,IYH
                    LD A,0
.pitch              EQU $-1
                    CALL rotate
                    ; [A', IYH, IXH] = rotate(rotate([x, y, z], OZ, roll), OX, pitch)

                    LD A,IYH
                    EX AF,AF'
                    LD IYL,A
                    LD IXL,IXH
                    LD A,0
.yaw                EQU $-1
                    NEG ; because x & z are swapped
                    CALL rotate
                    ; [IYH, A', IXH] = [x_rotated, y_rotated, z_rotated] = rotate(rotate(rotate([x, y, z], OZ, roll), OX, pitch), OY, yaw)

                    LD A,IXH
                    LD L,A
                    ADD A
                    SBC A
                    LD H,A
                    LD BC,0
.z_pos              EQU $-2
                    ADD HL,BC
                    EX DE,HL
                    ; DE = z_world = z_pos + z_rotated

                    PUSH DE ; saving z_world

                    EX AF,AF'
                    LD L,A
                    ADD A
                    SBC A
                    LD H,A
                    LD BC,0
.y_pos              EQU $-2
                    ADD HL,BC
                    ; HL = y_world = y_pos + y_rotated

                    CALL draw3D.project_y
                    EXX
                    ; HL' = y_screen = project(y_world, z_world)

                    POP DE ; restoring z_world

                    LD A,IYH
                    LD L,A
                    ADD A
                    SBC A
                    LD H,A
                    LD BC,0
.x_pos              EQU $-2
                    ADD HL,BC
                    ; HL = x_world = x_pos + x_rotated

                    JP draw3D.project_x ; HL = x_screen = project(x_world, z_world)


                    ALIGN 8
chess_wide
                    BYTE %11001100
                    BYTE %11001100
                    BYTE %00110011
                    BYTE %00110011
                    BYTE %11001100
                    BYTE %11001100
                    BYTE %00110011
                    BYTE %00110011
rabica
                    BYTE %00011000
                    BYTE %00100100
                    BYTE %01000010
                    BYTE %10000001
                    BYTE %10000001
                    BYTE %01000010
                    BYTE %00100100
                    BYTE %00011000


buffer              BLOCK 4 * 8

                    ENDMODULE
                    milestone
