; Generated tree.
; vertices - 41
; lines - 40
; min_x, max_x - -50, 45
; min_y, max_y - -60, 30
; min_z, max_z - -53, 39

draw
                    LD BC,#2900
                    LD HL,.vertices
                    CALL draw3D.set_vertices

                    LD HL,#0001
                    CALL draw3D.draw_line
                    LD HL,#0102
                    CALL draw3D.draw_line
                    LD HL,#0203
                    CALL draw3D.draw_line
                    LD HL,#0304
                    CALL draw3D.draw_line
                    LD HL,#0305
                    CALL draw3D.draw_line
                    LD HL,#0306
                    CALL draw3D.draw_line
                    LD HL,#0207
                    CALL draw3D.draw_line
                    LD HL,#0708
                    CALL draw3D.draw_line
                    LD HL,#0709
                    CALL draw3D.draw_line
                    LD HL,#070A
                    CALL draw3D.draw_line
                    LD HL,#020B
                    CALL draw3D.draw_line
                    LD HL,#0B0C
                    CALL draw3D.draw_line
                    LD HL,#0B0D
                    CALL draw3D.draw_line
                    LD HL,#0B0E
                    CALL draw3D.draw_line
                    LD HL,#010F
                    CALL draw3D.draw_line
                    LD HL,#0F10
                    CALL draw3D.draw_line
                    LD HL,#1011
                    CALL draw3D.draw_line
                    LD HL,#1012
                    CALL draw3D.draw_line
                    LD HL,#1013
                    CALL draw3D.draw_line
                    LD HL,#0F14
                    CALL draw3D.draw_line
                    LD HL,#1415
                    CALL draw3D.draw_line
                    LD HL,#1416
                    CALL draw3D.draw_line
                    LD HL,#1417
                    CALL draw3D.draw_line
                    LD HL,#0F18
                    CALL draw3D.draw_line
                    LD HL,#1819
                    CALL draw3D.draw_line
                    LD HL,#181A
                    CALL draw3D.draw_line
                    LD HL,#181B
                    CALL draw3D.draw_line
                    LD HL,#011C
                    CALL draw3D.draw_line
                    LD HL,#1C1D
                    CALL draw3D.draw_line
                    LD HL,#1D1E
                    CALL draw3D.draw_line
                    LD HL,#1D1F
                    CALL draw3D.draw_line
                    LD HL,#1D20
                    CALL draw3D.draw_line
                    LD HL,#1C21
                    CALL draw3D.draw_line
                    LD HL,#2122
                    CALL draw3D.draw_line
                    LD HL,#2123
                    CALL draw3D.draw_line
                    LD HL,#2124
                    CALL draw3D.draw_line
                    LD HL,#1C25
                    CALL draw3D.draw_line
                    LD HL,#2526
                    CALL draw3D.draw_line
                    LD HL,#2527
                    CALL draw3D.draw_line
                    LD HL,#2528
                    CALL draw3D.draw_line

                    LD A,(draw3D.yaw)
                    INC A
                    LD (draw3D.yaw),A

                    RET
.vertices
                    BYTE 0, -60, 0
                    BYTE 0, -12, 0
                    BYTE 0, 7, -23
                    BYTE 0, 1, -42
                    BYTE 0, -6, -52
                    BYTE 3, 0, -53
                    BYTE -3, 0, -51
                    BYTE 5, 20, -34
                    BYTE 7, 23, -43
                    BYTE 13, 24, -31
                    BYTE -1, 30, -36
                    BYTE -7, 19, -31
                    BYTE -10, 20, -38
                    BYTE -2, 30, -33
                    BYTE -16, 25, -29
                    BYTE 22, -4, 13
                    BYTE 34, 6, 5
                    BYTE 37, 10, 0
                    BYTE 43, 8, 2
                    BYTE 35, 15, 10
                    BYTE 35, -10, 20
                    BYTE 45, -10, 19
                    BYTE 42, -19, 24
                    BYTE 36, -7, 29
                    BYTE 25, 5, 24
                    BYTE 28, 11, 24
                    BYTE 30, -1, 32
                    BYTE 18, 8, 29
                    BYTE -23, 1, 13
                    BYTE -38, 13, 14
                    BYTE -43, 19, 5
                    BYTE -44, 19, 16
                    BYTE -47, 15, 15
                    BYTE -27, 14, 28
                    BYTE -31, 23, 26
                    BYTE -16, 17, 33
                    BYTE -33, 15, 39
                    BYTE -39, -3, 22
                    BYTE -50, 1, 17
                    BYTE -43, 0, 35
                    BYTE -41, -12, 23
