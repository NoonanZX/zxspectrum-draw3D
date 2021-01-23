                    milestone

;1.36 (fullscreen)
;1.28 (windowed)

draw
.x_min              EQU 50
.x_max              EQU 200
.y_min              EQU 50
.y_max              EQU 150

                    LD E,.x_min
                    LD D,.x_max
                    LD L,.y_min
                    LD H,.y_max
;                    CALL draw2DEX.set_viewport
;                    CALL .draw_viewport

                    LD BC,#0800
                    LD HL,.vertices
                    CALL draw3D.set_vertices

                    LD DE,.front
                    LD HL,draw2D.white
                    CALL draw3D.draw_polygon

                    LD DE,.back
                    LD HL,draw2D.white
                    CALL draw3D.draw_polygon

                    LD DE,.top
                    LD HL,.chess_wide
                    CALL draw3D.draw_polygon

                    LD DE,.bottom
                    LD HL,.rabica
                    CALL draw3D.draw_polygon

                    LD DE,.left
                    LD HL,draw2D.chess
                    CALL draw3D.draw_polygon

                    LD DE,.right
                    LD HL,draw2D.chess
                    CALL draw3D.draw_polygon

                    LD HL,draw3D.roll
                    INC (HL)
                    LD HL,draw3D.pitch
                    INC (HL)
                    LD HL,draw3D.yaw
                    INC (HL)

                    RET

.size_x             EQU 100
.size_y             EQU 100
.size_z             EQU 100

.vertices           BYTE -.size_x/2, -.size_y/2, +.size_z/2
                    BYTE +.size_x/2, -.size_y/2, +.size_z/2
                    BYTE -.size_x/2, +.size_y/2, +.size_z/2
                    BYTE +.size_x/2, +.size_y/2, +.size_z/2
                    BYTE -.size_x/2, -.size_y/2, -.size_z/2
                    BYTE +.size_x/2, -.size_y/2, -.size_z/2
                    BYTE -.size_x/2, +.size_y/2, -.size_z/2
                    BYTE +.size_x/2, +.size_y/2, -.size_z/2

.front              BYTE 4,5,7,6,-1 ; 0,0,+100
.back               BYTE 0,2,3,1,-1 ; 0,0,-100
.top                BYTE 6,7,3,2,-1 ; 0,+100,0
.bottom             BYTE 4,0,1,5,-1 ; 0,-100,0
.left               BYTE 0,4,6,2,-1 ; -100,0,0
.right              BYTE 5,1,3,7,-1 ; +100,0,0

                    ALIGN 8

.chess_wide         BYTE %11001100
                    BYTE %11001100
                    BYTE %00110011
                    BYTE %00110011
                    BYTE %11001100
                    BYTE %11001100
                    BYTE %00110011
                    BYTE %00110011

.rabica             BYTE %00011000
                    BYTE %00100100
                    BYTE %01000010
                    BYTE %10000001
                    BYTE %10000001
                    BYTE %01000010
                    BYTE %00100100
                    BYTE %00011000

.draw_viewport
                    LD D,.x_min
                    LD E,.y_min+1
                    LD H,.x_max
                    LD L,E
                    CALL draw2D.draw_horizontal_line

                    LD D,.x_min
                    LD E,.y_max-1
                    LD H,.x_max
                    LD L,E
                    CALL draw2D.draw_horizontal_line

                    LD D,.x_min+1
                    LD E,.y_min
                    LD H,D
                    LD L,.y_max
                    CALL draw2D.draw_vertical_line

                    LD D,.x_max-1
                    LD E,.y_min
                    LD H,D
                    LD L,.y_max
                    CALL draw2D.draw_vertical_line

                    RET

                    milestone
