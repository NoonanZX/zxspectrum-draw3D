                    MODULE draw2DEX

; Viewpoint (inclusive).
x_min               EQU test_point.x_min ; default = 0
y_min               EQU test_point.y_min ; default = 0
x_max               EQU test_point.x_max ; default = 255
y_max               EQU test_point.y_max ; default = 191


set_viewport
; BC - x_min, y_min
; DE - x_max, y_max
; Preserves ALL except A
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
; Preserves ALL except A
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


draw_point
; BC - x
; DE - y
                    CALL test_point
                    RET C

                    LD D,C
                    JP draw2D.draw_point

                    ENDMODULE
