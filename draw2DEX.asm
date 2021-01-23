                    milestone
                    MODULE draw2DEX

; Viewport: following symbols defined later using LUA:
;                    IFNDEF draw2DEX_viewport_no_x_min
;x_min               BYTE 0
;                    ENDIF
;                    IFNDEF draw2DEX_viewport_no_x_max
;x_max               BYTE 255
;                    ENDIF
;                    IFNDEF draw2DEX_viewport_no_y_min
;y_min               BYTE 0
;                    ENDIF
;y_max               BYTE 191


                    LUA ALLPASS
                        draw2DEX_viewport_addrs = { x_min = {}, x_max = {}, y_min = {}, y_max = {} }
                    ENDLUA

                    MACRO _draw2DEX_viewport_add_addr_1 border
                    ; Collects address ($-1) to store there new viewport border value when set_viewport is called.
                    ; This macro should me placed after LD A,<border_value> or CP <border_value> instructions.
                    ; All viewport addresses MUST be collected BEFORE set_viewport.
                    ; border - can be one of string literals {"xmin", "xmax", "ymin", "ymax"}
                        IF border == "xmin"
                            LUA ALLPASS
                                addrs = draw2DEX_viewport_addrs["x_min"]
                                addrs[#addrs + 1] = sj.current_address - 1
                            ENDLUA
                        ELSEIF border == "xmax"
                            LUA ALLPASS
                                addrs = draw2DEX_viewport_addrs["x_max"]
                                addrs[#addrs + 1] = sj.current_address - 1
                            ENDLUA
                        ELSEIF border == "ymin"
                            LUA ALLPASS
                                addrs = draw2DEX_viewport_addrs["y_min"]
                                addrs[#addrs + 1] = sj.current_address - 1
                            ENDLUA
                        ELSEIF border == "ymax"
                            LUA ALLPASS
                                addrs = draw2DEX_viewport_addrs["y_max"]
                                addrs[#addrs + 1] = sj.current_address - 1
                            ENDLUA
                        ENDIF
                    ENDM

                    MACRO _draw2DEX_viewport_add_addr_2 border
                    ; Collects address ($-2) to store there new viewport border value when set_viewport is called.
                    ; This macro should me placed after LD HL,<border_value> instructions.
                    ; All viewport addresses MUST be collected BEFORE set_viewport.
                    ; border - can be one of string literals {"xmin", "xmax", "ymin", "ymax"}
                        IF border == "xmin"
                            LUA ALLPASS
                                addrs = draw2DEX_viewport_addrs["x_min"]
                                addrs[#addrs + 1] = sj.current_address - 2
                            ENDLUA
                        ELSEIF border == "xmax"
                            LUA ALLPASS
                                addrs = draw2DEX_viewport_addrs["x_max"]
                                addrs[#addrs + 1] = sj.current_address - 2
                            ENDLUA
                        ELSEIF border == "ymin"
                            LUA ALLPASS
                                addrs = draw2DEX_viewport_addrs["y_min"]
                                addrs[#addrs + 1] = sj.current_address - 2
                            ENDLUA
                        ELSEIF border == "ymax"
                            LUA ALLPASS
                                addrs = draw2DEX_viewport_addrs["y_max"]
                                addrs[#addrs + 1] = sj.current_address - 2
                            ENDLUA
                        ENDIF
                    ENDM


draw_point
; BC - x
; DE - y
; Preserves IX, IY, ALL'.
                    LD A,B
                    OR D
                    RET NZ ; x<0 or x>255 or y<0 or y>255

                    IFNDEF draw2DEX_viewport_no_x_min
                        LD A,C
                        CP 0
                        _draw2DEX_viewport_add_addr_1 "xmin"
                        RET C ; x < x_min
                    ENDIF

                    IFNDEF draw2DEX_viewport_no_x_max
                        LD A,255
                        _draw2DEX_viewport_add_addr_1 "xmax"
                        CP C
                        RET C ; x > x_max
                    ENDIF

                    IFNDEF draw2DEX_viewport_no_y_min
                        LD A,E
                        CP 0
                        _draw2DEX_viewport_add_addr_1 "ymin"
                        RET C ; y < y_min
                    ENDIF

                    LD A,191
                    _draw2DEX_viewport_add_addr_1 "ymax"
                    CP E
                    RET C ; y > y_max

                    LD D,C
                    JP draw2D.draw_point


draw_line
; BC - x1
; DE - y1
; BC' - x2
; DE' - y2
; Preserves IX, IY.
                    CALL _clip_line_y
                    RET C
                    CALL _clip_line_x
                    RET C

                    LD D,C
                    JP draw2D.draw_line


draw_polygon
; Stack - [-1, x1, y1, x2, y2 ... xn, yn <= top]
; Points are ordered counterclockwise.
; Must be at least 3 points.
; HL - pattern_8x8
; Preserves IY.
                    LD (.pattern),HL
                    POP HL
                    LD (.ret_addr),HL

                    POP DE
                    POP BC
                    LD (.last_x),BC
                    LD (.last_y),DE
                    EXX
                    POP DE
                    POP BC
                    PUSH BC
                    PUSH DE
                    CALL .add_edge

                    POP DE
                    POP BC
                    EXX
                    POP DE
.loop               POP BC
                    PUSH BC
                    PUSH DE
                    CALL .add_edge

                    POP DE
                    POP BC
                    EXX
                    POP DE

                    LD A,D
                    AND E
                    INC A
                    JP NZ,.loop ; if DE <> -1 jump

                    LD BC,0
.last_x             EQU $-2
                    LD DE,0
.last_y             EQU $-2
                    CALL .add_edge

                    LD HL,0
.ret_addr           EQU $-2
                    PUSH HL
                    LD HL,0
.pattern            EQU $-2
                    JP draw2D.convex.fill
.add_edge
                    ; |(x1, y1) - (x2, y2)| = |(BC, DE) - (BC', DE')|
                    ; Edge going counterclockwise - so polygon`s inner area is to the left.

                    CALL _clip_line_y
                    RET C ; If max(y1, y2) < y_min or min(y1, y2) > y_max then just ignoring edge.
                    ; |(xx1, yy1) - (xx2, yy2)| = |(x1, y1) - (x2, y2)| x [y_min, y_max] = |(BC, E) - (BC', A = E')|.

                    MACRO _draw2DEX_draw_polygon_add_edge dir_y
                        ; |(xx1, yy1) - (xx2, yy2)| = |(BC, IXL = E) - (BC', IXH = E')|.
                        ; if y1 < y2 (dir_y=+1) - this line is a polygon`s LEFT border.
                        ; if y1 > y2 (dir_y=-1) - this line is a polygon`s RIGHT border.
                        ; Case y1 == y2 handled properly by (dir_y=+1) code variant.

                        CALL _clip_line_x
                        JR C,.outside ; if (max(xx1, xx2) < x_min or min(xx1, xx2) > x_max) jump
                        ; |(xxx1, yyy1) - (xxx2, yyy2)| = |(xx1, yy1) - (xx2, yy2)| x [x_min, x_max] = |(C, E) - (H, L)|

                        ; Testing if |(xx1, yy1) - (xx2, yy2)| crosses appropriate viewport border - LEFT (x_min) if y1 < y2 or RIGHT (x_max) if y1 > y2.
                        LD A,0
                        if_then_else dir_y>0, _draw2DEX_viewport_add_addr_1 "xmin", _draw2DEX_viewport_add_addr_1 "xmax"
                        CP C
                        JR Z,.hit_begin
                        CP H
                        JR Z,.hit_end

                        ; draw2D.convex.add_*** procedures works with CLOCKWISE-oriented border parts, so dont forget to reverse our lines.
.inside
                        ; Adding |(xxx2, yyy2) - (xxx1, yyy1)|.
                        LD D,C
                        EX DE,HL
                        JP draw2D.convex.add_line
.hit_begin
                        CP H
                        JR Z,.outside_ok ; if (xx1 == xx2) jump

                        ; Adding |(xxx2, yyy2) - (xxx1, yyy1)|.
                        LD D,C
                        EX DE,HL
                        CALL draw2D.convex.add_line

                        ; Adding |(xxx1, yyy1) - (xxx1, yy1)|.
                        LD H,D
                        LD A,IXL
                        LD L,A
                        JP draw2D.convex.add_vertical_line
.hit_end
                        LD D,C
                        PUSH DE ; (xxx1, yyy1) =>

                        ; Adding |(xxx2, yy2) - (xxx2, yyy2)|.
                        LD D,H
                        LD E,IXH
                        CALL draw2D.convex.add_vertical_line

                        ; Adding |(xxx2, yyy2) - (xxx1, yyy1)|.
                        POP HL ; (xxx1, yyy1) <=
                        JP draw2D.convex.add_line
.outside
                        OR A
                        if_then_else dir_y>0, RET NZ, RET Z ; If LEFT/RIGHT polygon`s border is RIGHT/LEFT from viewport - just ignore it.
.outside_ok
                        ; Adding |(x_min/x_max, yy2) - (x_min/x_max, yy1)|.
                        LD D,0
                        if_then_else dir_y>0, _draw2DEX_viewport_add_addr_1 "xmin", _draw2DEX_viewport_add_addr_1 "xmax"
                        LD E,IXH
                        LD H,D
                        LD A,IXL
                        LD L,A
                        JP draw2D.convex.add_vertical_line
                    ENDM

                    CP E
                    LD IXL,E ; IXL = y1
                    LD IXH,A ; IXH = y2
                    JP C,.y1_gt_y2

.y1_le_y2           _draw2DEX_draw_polygon_add_edge +1
.y1_gt_y2           _draw2DEX_draw_polygon_add_edge -1


_clip_line_y
; Returns part of line inside [y_min, y_max] or CF=1 if line is completely outside.
; BC - x1
; DE - y1
; BC' - x2
; DE' - y2
; Output:
; Success
;     CF = 0
;     BC = new_x1
;     DE = new_y1
;     BC' = new_x2
;     DE' = new_y2
;     D = D' = 0
;     A = E' = new_y2
; Failure
;     CF = 1
;     A == 0 if max(y1, y2) < y_min
;     A <> 0 if min(y1, y2) > y_max
;     BC, DE, BC', DE' preserved
; Preserves IX, IY.
                    MACRO _draw2DEX_clip_line_y_test exit_path, ge_0_and_lt_min, gt_max_and_le_255, lt_0_or_gt_255
                    ; Tests y against y_min and y_max and does following:
                    ;     y in [y_min, y_max] => fall through
                    ;     y in [0, y_min)     => jump ge_0_and_lt_min or return if label is 0
                    ;     y in (y_max, 255]   => jump gt_max_and_le_255 or return if label is 0
                    ;     y not in [0, 255]   => jump lt_0_or_gt_255
                    ; exit_path argument exists only because we can`t use forward labels in IF preprocessor statements.
                    ;     exit_path < 0 means ge_0_and_lt_min == 0, gt_max_and_le_255 <> 0
                    ;     exit_path > 0 means ge_0_and_lt_min <> 0, gt_max_and_le_255 == 0
                    ;     exit_path = 0 means ge_0_and_lt_min <> 0, gt_max_and_le_255 <> 0
                    ; Input: DE - y
                    ; Output:
                    ; If exit_path < 0 and y < y_min
                    ;     return with A==0
                    ; If exit_path > 0 and y > y_max
                    ;     return with A<>0
                    ; Preserves ALL except AF.
                        XOR A
                        if_then_else exit_path>0, SUB D, CP D
                        JP C,lt_0_or_gt_255 ; y<0 or y>255 => jump with [A=0 if exit_path<0, A<>0 if exit_path>0, SF=0 if y<0, SF=1 if y>255]

                        IFNDEF draw2DEX_viewport_no_y_min
                            LD A,E
                            CP 0
                            _draw2DEX_viewport_add_addr_1 "ymin"
                            if_then exit_path<0, <LD A,D> ; D == 0
                            if_then_else exit_path<0, RET C, <JP C,ge_0_and_lt_min> ; y<y_min => if exit_path<0 return [A=0] else jump
                        ENDIF

                        LD A,191
                        _draw2DEX_viewport_add_addr_1 "ymax"
                        SUB E
                        if_then_else exit_path>0, RET C, <JP C,gt_max_and_le_255> ; y>y_max => if exit_path>0 return [A<>0] else jump
                    ENDM

                    MACRO _draw2DEX_clip_line_y_calc_dy get_result
                    ; BC,  DE  - x1, y1
                    ; BC', DE' - x2, y2
                    ; Output:
                    ; BC <=> BC'
                    ; DE <=> DE'
                    ; HL' <= HL
                    ; HL  = y1 - y2
                    ; ZF  = {(y1 - y2) < 256}
                    ; If ZF == 1 and get_result
                    ;     A = L = y1 - y2
                    ; Preserves AF', IX, IY.
                        PUSH DE
                        EXX
                        POP HL
                        XOR A
                        SBC HL,DE
                        ; HL  = y1 - y2
                        ; A = 0
                        SUB H
                        ; ZF  = {(y1 - y2) < 256}
                        if_then get_result, <LD A,L>
                    ENDM

                    MACRO _draw2DEX_clip_line_y_calc_dx get_result
                    ; BC,  DE  - x1, y1
                    ; BC', DE' - x2, y2
                    ; Output:
                    ; BC <=> BC'
                    ; DE <=> DE'
                    ; HL' <= HL
                    ; HL  = x1 - x2
                    ; ZF  = {-128 <= (x1 - x2) <= 127}
                    ; If ZF == 1 and get_result
                    ;     A = L = x1 - x2
                    ; Preserves AF', IX, IY.
                        PUSH BC
                        EXX
                        POP HL
                        OR A
                        SBC HL,BC
                        ; HL  = x1 - x2
                        LD A,L
                        RLCA
                        AND 1
                        ADD H
                        ; ZF  = {-128 <= (x1 - x2) <= 127}
                        if_then get_result, <LD A,L>
                    ENDM

                    MACRO _draw2DEX_clip_line_y_1 dir_y
                    ; Finds (x_new,y_new) - intersection of line |(x1,y1)-(x2,y2)| with y=y_min or y=y_max (depending on dir_y) and returns |(x_new,y_new)-(x2, y2)|.
                    ; dir_y - sign(y2 - y1)
                    ; BC - x1
                    ; DE - y1
                    ; If y1 < y2
                    ;     A - (x1 - x2)
                    ;     L - (y2 - y1)
                    ; If y1 > y2:
                    ;     A - (y1 - y2)
                    ;     L - (x2 - x1)
                    ; Output:
                    ; If y1 < y2:
                    ;     DE = y_new = y_min
                    ;     BC = x_new = x1 + (x2 - x1) * (y_min - y1) / (y2 - y1)
                    ; If y1 > y2:
                    ;     DE = y_new = y_max
                    ;     BC = x_new = x1 + (x2 - x1) * (y1 - y_max) / (y1 - y2)
                    ; Preserves IX, IY, ALL'.
                        PUSH BC ; store x1

                        IF dir_y > 0
                            LD B,A
                        ELSE
                            LD B,L
                            LD L,A
                        ENDIF
                        ; B = -/+dx = -/+(x2 - x1)
                        ; L = dy = abs(y1 - y2)

                        ; Calculating distance to border.
                        IF dir_y > 0
                            LD A,0
                            _draw2DEX_viewport_add_addr_1 "ymin"
                            SUB E
                            ; A = distance = y_min - y1
                        ELSE
                            LD A,E
                            SUB 191
                            _draw2DEX_viewport_add_addr_1 "ymax"
                            ; A = distance = y1 - y_max
                        ENDIF

                        LD D,A ; D = distance
                        LD E,L ; E = dy
                        CALL div_256uD_uE_A
                        ; A = 256 * t = 256 * distance / dy

                        ; B = -/+dx
                        LD C,A ; C = 256 * t
                        CALL mul_sB_uC_HL
                        ; H = -/+dx * t

                        IF dir_y>0
                            XOR A
                            SUB H
                        ELSE
                            LD A,H
                        ENDIF
                        ; A = dx * t

                        LD L,A
                        ADD A
                        SBC A
                        LD H,A
                        ; HL = dx * t

                        POP BC ; restore x1
                        ADD HL,BC
                        LD B,H
                        LD C,L
                        ; BC = new_x = x1 + dx * t

                        if_then_else dir_y>0, <LD DE,0>, <LD DE,191>
                        if_then_else dir_y>0, _draw2DEX_viewport_add_addr_2 "ymin", _draw2DEX_viewport_add_addr_2 "ymax"
                        ; DE = new_y = y_min/y_max
                    ENDM

                    MACRO _draw2DEX_clip_line_y clip_begin, clip_end, dir_y
                    ; Clip lines begin if clip_begin<>0 and end if clip_end<>0.
                    ; dir_y = sign(y2 - y1) - y1 never equals y2, so dir_y = +/-1
                    ; BC, DE   - x2, y2
                    ; BC', DE' - x1, y1
                    ; Note that line is inverted!
                    ; Special - makes RET!
                    ; Preserves IX, IY.
.restart
                        ; Calculating dy = y2 - y1, dx = x1 - x2 (if y1 < y2) or dy = y1 - y2, dx = x2 - x1 (if y1 > y2).
                        ; Note that dy is always positive, while dx can have any value.
                        ; Simltaneously we checking that dy < 256 and -128 <= dx <= 127.
                        if_then_else dir_y>0, _draw2DEX_clip_line_y_calc_dy clip_end, _draw2DEX_clip_line_y_calc_dx clip_end
                        EX AF,AF'
                        if_then_else dir_y>0, _draw2DEX_clip_line_y_calc_dx clip_begin, _draw2DEX_clip_line_y_calc_dy clip_begin
                        ; If y1 < y2
                        ;     HL  = dx = x1 - x2
                        ;     HL' = dy = y2 - y1
                        ;     ZF  = {-128 <= dx <= 127}
                        ;     ZF' = {dy < 256}
                        ; If y1 > y2
                        ;     HL  = dy = y1 - y2
                        ;     HL' = dx = x2 - x1
                        ;     ZF  = {dy < 256}
                        ;     ZF' = {-128 <= dx <= 127}
                        ; If clip_begin A  = L
                        ; If clip_end   A' = L'

                        ; If dy < 256 and -128 <= dx <= 127 we can do everything in one pass, otherwise we have to split line into two parts and proceed recursively.
                        JR NZ,.split
                        EX AF,AF'
                        JR NZ,.split
                        ; Note that A <=> A' so both A,L and A',L' now contains dx and dy, although in different order.

                        ; Clip line end if needed.
                        IF clip_end
                            _draw2DEX_clip_line_y_1 -dir_y
                        ENDIF

                        LD A,E ; copying y2
                        EXX

                        ; Clip line begin if needed.
                        IF clip_begin
                            EX AF,AF'
                            _draw2DEX_clip_line_y_1 +dir_y
                            EX AF,AF'
                        ENDIF

                        ; All done.
                        OR A
                        RET
.split
                        PUSH BC ; x2
                        PUSH DE ; y2

                        ; If y1 < y2 calculating (x1 + x2) / 2
                        ; If y1 > y2 calculating (y1 + y2) / 2
                        if_then_else dir_y>0, SRA H, SRL H
                        RR L
                        if_then_else dir_y>0, <ADD HL,BC>, <ADD HL,DE>
                        IF dir_y > 0
                            LD B,H
                            LD C,L
                        ELSE
                            EX DE,HL
                        ENDIF

                        ; If y1 < y2 calculating (y1 + y2) / 2
                        ; If y1 > y2 calculating (x1 + x2) / 2
                        EXX
                        if_then_else dir_y<0, SRA H, SRL H
                        RR L
                        if_then_else dir_y<0, <ADD HL,BC>, <ADD HL,DE>
                        PUSH HL
                        EXX
                        if_then_else dir_y<0, POP BC, POP DE

                        ; BC = xm = (x1 + x2) / 2
                        ; DE = ym = (y1 + y2) / 2

                        ; Now we test ym against [y_min, y_max] and (taking into account macro arguments) will choose what to do with line parts: KEEP, CLIP or DROP.
                        ; (x1, y1) -- (xm, ym) | clip_begin = 1  | clip_begin = 0  | clip_begin = 1  | clip_begin = 1  | clip_begin = 0  | clip_begin = 1  |
                        ;          /           | clip_end   = 0  | clip_end   = 1  | clip_end   = 1  | clip_end   = 0  | clip_end   = 1  | clip_end   = 1  |
                        ; (xm, ym) -- (x2, y2) | y_dir>0 (y1<y2) | y_dir>0 (y1<y2) | y_dir>0 (y1<y2) | y_dir<0 (y1>y2) | y_dir<0 (y1>y2) | y_dir<0 (y1>y2) |
                        ;----------------------+-----------------+-----------------+-----------------+-----------------+-----------------+-----------------+
                        ;      ym < y_min      |   drop / clip   |                 |   drop / clip   |                 |   clip / drop   |   clip / drop   |
                        ; y_min <= ym <= y_max |   clip / keep   |   keep / clip   |   clip / clip   |   clip / keep   |   keep / clip   |   clip / clip   |
                        ;      ym > y_max      |                 |   clip / drop   |   clip / drop   |   drop / clip   |                 |   drop / clip   |
                        ;----------------------+-----------------+-----------------+-----------------+-----------------+-----------------+-----------------+
                        ; Blank fields left for impossible combinations.
                        ; We have five cases: clip/clip, clip/keep, keep/clip, clip/drop, drop/clip.

                        XOR A
                        CP D
                        JR C,.split_drop
                        IFNDEF draw2DEX_viewport_no_y_min
                            IF dir_y>0 && clip_begin || dir_y<0 && clip_end
                                LD A,(y_min)
                                LD L,A
                                LD A,E
                                CP L
                                if_then_else dir_y>0, <JR C,.split_drop_clip>, <JR C,.split_clip_drop>
                            ENDIF
                        ENDIF
                        IF dir_y>0 && clip_end || dir_y<0 && clip_begin
                            LD A,(y_max)
                            CP E
                            if_then_else dir_y>0, <JR C,.split_clip_drop>, <JR C,.split_drop_clip>
                        ENDIF

                        ; In all cases except clip/clip we can .restart this macro because we do a single clip with same <clip_begin, clip_end> arguments.
                        ; But in case of clip/clip we have to call .y1_lt__y2_ok/.y1_ok__y2_gt or .y1_gt__y2_ok/.y1_ok__y2_lt
                        ; to clip first and second line parts with <clip_begin=1, clip_end=0> and <clip_begin=0, clip_end=1> arguments respectively.

                        ; BC, DE       = xm, ym
                        ; BC', DE'     = x1, y1
                        ; (SP+2), (SP) = x2, y2

                        IF clip_begin && clip_end
.split_clip_clip            if_then_else dir_y>0, <CALL .y1_lt__y2_ok>, <CALL .y1_gt__y2_ok>
                            ; BC, DE       = new_x1, new_y1
                            ; BC', DE'     = xm, ym
                            ; (SP+2), (SP) = x2, y2
                            POP HL
                            EX DE,HL
                            EX (SP),HL
                            PUSH BC
                            LD B,H
                            LD C,L
                            ; BC', DE'     = xm, ym
                            ; BC, DE       = x2, y2
                            ; (SP), (SP+2) = new_x1, new_y1
                            if_then_else dir_y>0, <CALL .y1_ok__y2_gt>, <CALL .y1_ok__y2_lt>
                            ; BC', DE'     = new_x2, new_y2
                            ; (SP), (SP+2) = new_x1, new_y1
                            POP BC
                            POP DE
                            ; BC, DE   = new_x1, new_y1
                            ; BC', DE' = new_x2, new_y2
                            RET
                        ELSEIF clip_begin
.split_clip_keep            CALL .restart
                            ; BC, DE       = new_x1, new_y1
                            ; (SP+2), (SP) = x2, y2
                            EXX
                            POP DE
                            POP BC
                            LD A,E ; copying y2
                            EXX
                            ; BC, DE   = new_x1, new_y1
                            ; BC', DE' = x2, y2
                            RET
                        ELSE
.split_keep_clip            EXX
                            POP HL
                            EX DE,HL
                            EX (SP),HL
                            PUSH BC
                            LD B,H
                            LD C,L
                            ; BC', DE'     = xm, ym
                            ; BC, DE       = x2, y2
                            ; (SP), (SP+2) = x1, y1
                            CALL .restart
                            ; BC', DE'     = new_x2, new_y2
                            ; (SP), (SP+2) = x1, y1
                            POP BC
                            POP DE
                            ; BC, DE   = x1, y1
                            ; BC', DE' = new_x2, new_y2
                            RET
                        ENDIF
.split_drop
                        IF clip_begin && clip_end
                            if_then_else dir_y>0, <JP P,.split_drop_clip>, <JP M,.split_drop_clip>
                        ENDIF
                        IF clip_end
.split_clip_drop            POP HL
                            POP HL
                            ; BC', DE' = x1, y1
                            ; BC, DE   = xm, ym
                            JP .restart
                        ENDIF
                        IF clip_begin
.split_drop_clip            EXX
                            POP DE
                            POP BC
                            ; BC', DE' = xm, ym
                            ; BC, DE   = x2, y2
                            JP .restart
                        ENDIF
                    ENDM

                    ; First we test both line points with _draw2DEX_clip_line_y_test, then clipping is done with _draw2DEX_clip_line_y if needed.
                    ; We have 9 cases:
                    ;                      |      y1 < y_min      | y_min <= y1 <= y_max |      y1 > y_max
                    ;----------------------+----------------------+----------------------+-------------------------------
                    ;                      |       outside        | clip_begin = 0       | clip_begin = 1
                    ;      y2 < y_min      |         RET          | clip_ end  = 1       | clip_end   = 1
                    ;                      |   (CF = 1, A = 0)    | dir_y = -1 (y1 > y2) | dir_y = -1 (y1 > y2)
                    ;----------------------+----------------------+----------------------+-------------------------------
                    ;                      | clip_begin = 1       |        inside        | clip_begin = 1
                    ; y_min <= y2 <= y_max | clip_end   = 0       |         RET          | clip_end   = 0
                    ;                      | dir_y = +1 (y1 < y2) |       (CF = 0)       | dir_y = -1 (y1 > y2)
                    ;----------------------+----------------------+----------------------+-------------------------------
                    ;                      | clip_begin = 1       | clip_begin = 0       |       outside
                    ;      y2 > y_max      | clip_end   = 1       | clip_ end  = 1       |         RET
                    ;                      | dir_y = +1 (y1 < y2) | dir_y = +1 (y1 < y2) |   (CF = 1, A <> 0)
                    ;----------------------+----------------------+----------------------+-------------------------------
                    ; Note that if y1 == y2 no clipping required, because whole line is completely inside or outside [y_min, y_max].

                    _draw2DEX_clip_line_y_test 0, .y1_lt, .y1_gt, .y1_out_0_FF
.y1_ok              EXX
                    _draw2DEX_clip_line_y_test 0, .y1_ok__y2_lt, .y1_ok__y2_gt, .y1_ok__y2_out_0_FF
.y1_ok__y2_ok       ; No clipping needed.
                    LD A,E ; copying y2
                    EXX
                    RET

.y1_ok__y2_out_0_FF JP M,.y1_ok__y2_gt ; SF == 1 if y2 > 0
.y1_ok__y2_lt       _draw2DEX_clip_line_y 0, 1, -1
.y1_ok__y2_gt       _draw2DEX_clip_line_y 0, 1, +1

.y1_out_0_FF        JP M,.y1_gt ; SF == 1 if y1 > 0

.y1_lt              EXX
                    _draw2DEX_clip_line_y_test -1, 0, .y1_lt__y2_gt, .y1_lt__y2_out_0_FF
.y1_lt__y2_ok       _draw2DEX_clip_line_y 1, 0, +1
.y1_lt__y2_out_0_FF RET P ; SF == 0 if y2 < 0
.y1_lt__y2_gt       _draw2DEX_clip_line_y 1, 1, +1

.y1_gt              EXX
                    _draw2DEX_clip_line_y_test +1, .y1_gt__y2_lt, 0, .y1_gt__y2_out_0_FF
.y1_gt__y2_ok       _draw2DEX_clip_line_y 1, 0, -1
.y1_gt__y2_out_0_FF RET M ; SF == 1 if y2 > 0
.y1_gt__y2_lt       _draw2DEX_clip_line_y 1, 1, -1


_clip_line_x
; Returns part of line inside [x_min, x_max] or CF=1 if line is completely outside.
; Requires y_min <= y1 <= y_max and y_min <= y2 <= y_max.
; BC - x1
; DE - y1
; BC' - x2
; DE' - y2
; Output:
; Success
;     CF = 0
;     BC = new_x1
;     DE = new_y1
;     BC' = new_x2
;     DE' = new_y2
;     B = D = B' = D' = 0
;     H = C' = new_x2
;     L = E' = new_y2
; Failure
;     CF = 1
;     A == 0 if max(x1, x2) < x_min
;     A <> 0 if min(x1, x2) > x_max
;     BC, DE, BC', DE' preserved
; Preserves IX, IY.
                    MACRO _draw2DEX_clip_line_x_test exit_path, ge_0_and_lt_min, gt_max_and_le_255, lt_0_or_gt_255
                    ; Tests x against x_min and x_max and does following:
                    ;     x in [x_min, x_max] => fall through
                    ;     x in [0, x_min)     => jump ge_0_and_lt_min or return if label is 0
                    ;     x in (x_max, 255]   => jump gt_max_and_le_255 or return if label is 0
                    ;     x not in [0, 255]   => jump lt_0_or_gt_255
                    ; exit_path argument exists only because we can`t use forward labels in IF preprocessor statements.
                    ;     exit_path < 0 means ge_0_and_lt_min == 0, gt_max_and_le_255 <> 0
                    ;     exit_path > 0 means ge_0_and_lt_min <> 0, gt_max_and_le_255 == 0
                    ;     exit_path = 0 means ge_0_and_lt_min <> 0, gt_max_and_le_255 <> 0
                    ; Input: BC - x
                    ; Output:
                    ; If exit_path < 0 and x < x_min
                    ;     return with A==0
                    ; If exit_path > 0 and x > x_max
                    ;     return with A<>0
                    ; Preserves ALL except AF.
                        XOR A
                        if_then_else exit_path>0, SUB B, CP B
                        JP C,lt_0_or_gt_255 ; x<0 or x>255 => jump with [A=0 if exit_path<0, A<>0 if exit_path>0, SF=0 if x<0, SF=1 if x>255]

                        IFNDEF draw2DEX_viewport_no_x_min
                            LD A,C
                            CP 0
                            _draw2DEX_viewport_add_addr_1 "xmin"
                            if_then exit_path<0, <LD A,B> ; B == 0
                            if_then_else exit_path<0, RET C, <JP C,ge_0_and_lt_min> ; x<x_min => if exit_path<0 return [A=0] else jump
                        ENDIF

                        IFNDEF draw2DEX_viewport_no_x_max
                            LD A,255
                            _draw2DEX_viewport_add_addr_1 "xmax"
                            SUB C
                            if_then_else exit_path>0, RET C, <JP C,gt_max_and_le_255> ; x>x_max => if exit_path>0 return [A<>0] else jump
                        ENDIF
                    ENDM

                    MACRO _draw2DEX_clip_line_x_copy_xy
                    ; C - x
                    ; E - y
                    ; Output:
                    ; BC <=> BC'
                    ; DE <=> DE'
                    ; HL' <= HL
                    ; HL = x, y
                    ; Preserves IX, IY.
                        LD A,C
                        EX AF,AF'
                        LD A,E
                        EXX
                        LD L,A
                        EX AF,AF'
                        LD H,A
                    ENDM

                    MACRO _draw2DEX_clip_line_x_calc_dx get_result
                    ; BC,  DE  - x1, y1
                    ; BC', DE' - x2, y2
                    ; Output:
                    ; BC <=> BC'
                    ; DE <=> DE'
                    ; HL' <= HL
                    ; HL  = x1 - x2
                    ; ZF  = {(x1 - x2) < 256}
                    ; If ZF == 1 and get_result
                    ;     A = L = x1 - x2
                    ; Preserves AF', IX, IY.
                        PUSH BC
                        EXX
                        POP HL
                        XOR A
                        SBC HL,BC
                        ; HL  = x1 - x2
                        ; A = 0
                        SUB H
                        ; ZF  = {(x1 - x2) < 256}
                        if_then get_result, <LD A,L>
                    ENDM

                    MACRO _draw2DEX_clip_line_x_calc_dy get_result
                    ; BC,  DE  - x1, y1
                    ; BC', DE' - x2, y2
                    ; Requires D = D' = 0.
                    ; Output:
                    ; BC <=> BC'
                    ; DE <=> DE'
                    ; HL' <= HL
                    ; HL  = y1 - y2
                    ; SF  = ~{-128 <= (y1 - y2) <= 127}
                    ; If SF == 0 and get_result
                    ;     A = L = y1 - y2
                    ; Preserves AF', IX, IY.
                        LD A,E
                        EXX
                        SUB E
                        LD L,A
                        SBC A
                        LD H,A
                        ; HL  = y1 - y2
                        ; A = y1 > y2 ? 0 : -1
                        XOR L
                        ; SF  = ~{-128 <= (y1 - y2) <= 127}
                        if_then get_result, <LD A,L>
                    ENDM

                    MACRO _draw2DEX_clip_line_x_1 dir_x
                    ; Finds (x_new,y_new) - intersection of line |(x1,y1)-(x2,y2)| with x=x_min or x=x_max (depending on dir_x) and returns |(x_new,y_new)-(x2, y2)|.
                    ; dir_x - sign(x2 - x1)
                    ; BC - x1
                    ; DE - y1
                    ; If x1 < x2
                    ;     A - (y1 - y2)
                    ;     L - (x2 - x1)
                    ; If x1 > x2:
                    ;     A - (x1 - x2)
                    ;     L - (y2 - y1)
                    ; Output:
                    ; If x1 < x2:
                    ;     BC = x_new = x_min
                    ;     DE = y_new = y1 + (y2 - y1) * (x_min - x1) / (x2 - x1)
                    ; If x1 > x2:
                    ;     BC = x_new = x_max
                    ;     DE = y_new = y1 + (y2 - y1) * (x1 - x_max) / (x1 - x2)
                    ; Preserves IX, IY, ALL'.
                        PUSH DE ; store y1

                        IF dir_x > 0
                            LD B,A
                        ELSE
                            LD B,L
                            LD L,A
                        ENDIF
                        ; B = -/+dy = -/+(y2 - y1)
                        ; L = dx = abs(x1 - x2)

                        ; Calculating distance to border.
                        IF dir_x > 0
                            LD A,0
                            _draw2DEX_viewport_add_addr_1 "xmin"
                            SUB C
                            ; A = distance = x_min - x1
                        ELSE
                            LD A,C
                            SUB 255
                            _draw2DEX_viewport_add_addr_1 "xmax"
                            ; A = distance = x1 - x_max
                        ENDIF

                        LD D,A ; D = distance
                        LD E,L ; E = dx
                        CALL div_256uD_uE_A
                        ; A = 256 * t = 256 * distance / dx

                        ; B = -/+dy
                        LD C,A ; C = 256 * t
                        CALL mul_sB_uC_HL
                        ; H = -/+dy * t

                        IF dir_x>0
                            XOR A
                            SUB H
                        ELSE
                            LD A,H
                        ENDIF
                        ; A = dy * t

                        LD L,A
                        ADD A
                        SBC A
                        LD H,A
                        ; HL = dy * t

                        POP DE ; restore y1
                        ADD HL,DE
                        EX DE,HL
                        ; DE = new_y = y1 + dy * t

                        if_then_else dir_x>0, <LD BC,0>, <LD BC,255>
                        if_then_else dir_x>0, _draw2DEX_viewport_add_addr_2 "xmin", _draw2DEX_viewport_add_addr_2 "xmax"
                        ; BC = new_x = x_min/x_max
                    ENDM

                    MACRO _draw2DEX_clip_line_x clip_begin, clip_end, dir_x
                    ; Clip lines begin if clip_begin<>0 and end if clip_end<>0.
                    ; dir_x = sign(x2 - x1) - x1 never equals x2, so dir_x = +/-1
                    ; BC, DE   - x2, y2
                    ; BC', DE' - x1, y1
                    ; Requires D = D' = 0.
                    ; Note that line is inverted!
                    ; Special - makes RET!
                    ; Preserves IX, IY.
.restart
                        ; Calculating dx = x2 - x1, dy = y1 - y2 (if x1 < x2) or dx = x1 - x2, dx = y2 - y1 (if x1 > x2).
                        ; Note that dx is always positive, while dy can have any value.
                        ; Simltaneously we checking that dx < 256 and -128 <= dy <= 127.
                        if_then_else dir_x>0, _draw2DEX_clip_line_x_calc_dx clip_end, _draw2DEX_clip_line_x_calc_dy clip_end
                        EX AF,AF'
                        if_then_else dir_x>0, _draw2DEX_clip_line_x_calc_dy clip_begin, _draw2DEX_clip_line_x_calc_dx clip_begin
                        ; If x1 < x2
                        ;     HL  = dy = y1 - y2
                        ;     HL' = dx = x2 - x1
                        ;     SF  = ~{-128 <= dy <= 127}
                        ;     ZF' = {dx < 256}
                        ; If x1 > x2
                        ;     HL  = dx = x1 - x2
                        ;     HL' = dy = y2 - y1
                        ;     ZF  = {dx < 256}
                        ;     SF' = ~{-128 <= dy <= 127}
                        ; If clip_begin A  = L
                        ; If clip_end   A' = L'

                        ; If dx < 256 and -128 <= dy <= 127 we can do everything in one pass, otherwise we have to split line into two parts and proceed recursively.
                        if_then_else dir_x>0, <JP M,.split>, <JR NZ,.split>
                        EX AF,AF'
                        if_then_else dir_x>0, <JR NZ,.split>, <JP M,.split>
                        ; Note that A <=> A' so both A,L and A',L' now contains dx and dy, although in different order.

                        ; Clip line end if needed.
                        IF clip_end
                            _draw2DEX_clip_line_x_1 -dir_x
                        ENDIF

                        ; Clip line begin if needed.
                        IF clip_begin
                            ; We will need some registers, so using PUSH/POP instead of slightly faster _draw2DEX_clip_line_x_copy_xy.
                            LD D,C
                            PUSH DE ; x2, y2
                            LD D,B ; B == 0

                            EXX
                            EX AF,AF'

                            _draw2DEX_clip_line_x_1 +dir_x

                            POP HL ; x2, y2
                        ELSE
                            _draw2DEX_clip_line_x_copy_xy
                        ENDIF

                        ; All done.
                        OR A
                        RET
.split
                        PUSH BC ; x2
                        PUSH DE ; y2

                        ; If x1 < x2 calculating (y1 + y2) / 2
                        ; If x1 > x2 calculating (x1 + x2) / 2
                        if_then_else dir_x>0, SRA H, SRL H
                        RR L
                        if_then_else dir_x>0, <ADD HL,DE>, <ADD HL,BC>
                        IF dir_x > 0
                            EX DE,HL
                        ELSE
                            LD B,H
                            LD C,L
                        ENDIF

                        ; If x1 < x2 calculating (x1 + x2) / 2
                        ; If x1 > x2 calculating (y1 + y2) / 2
                        EXX
                        if_then_else dir_x<0, SRA H, SRL H
                        RR L
                        if_then_else dir_x<0, <ADD HL,DE>, <ADD HL,BC>
                        PUSH HL
                        EXX
                        if_then_else dir_x<0, POP DE, POP BC

                        ; BC = xm = (x1 + x2) / 2
                        ; DE = ym = (y1 + y2) / 2

                        ; Now we test xm against [x_min, x_max] and (taking into account macro arguments) will choose what to do with line parts: KEEP, CLIP or DROP.
                        ; (x1, y1) -- (xm, ym) | clip_begin = 1  | clip_begin = 0  | clip_begin = 1  | clip_begin = 1  | clip_begin = 0  | clip_begin = 1  |
                        ;          /           | clip_end   = 0  | clip_end   = 1  | clip_end   = 1  | clip_end   = 0  | clip_end   = 1  | clip_end   = 1  |
                        ; (xm, ym) -- (x2, y2) | x_dir>0 (x1<yx) | x_dir>0 (x1<yx) | x_dir>0 (x1<x2) | x_dir<0 (x1>x2) | x_dir<0 (x1>x2) | x_dir<0 (x1>x2) |
                        ;----------------------+-----------------+-----------------+-----------------+-----------------+-----------------+-----------------+
                        ;      xm < x_min      |   drop / clip   |                 |   drop / clip   |                 |   clip / drop   |   clip / drop   |
                        ; x_min <= xm <= x_max |   clip / keep   |   keep / clip   |   clip / clip   |   clip / keep   |   keep / clip   |   clip / clip   |
                        ;      xm > x_max      |                 |   clip / drop   |   clip / drop   |   drop / clip   |                 |   drop / clip   |
                        ;----------------------+-----------------+-----------------+-----------------+-----------------+-----------------+-----------------+
                        ; Blank fields left for impossible combinations.
                        ; We have five cases: clip/clip, clip/keep, keep/clip, clip/drop, drop/clip.

                        XOR A
                        CP B
                        JR C,.split_drop
                        IFNDEF draw2DEX_viewport_no_x_min
                            IF dir_x>0 && clip_begin || dir_x<0 && clip_end
                                LD A,(x_min)
                                LD L,A
                                LD A,C
                                CP L
                                if_then_else dir_x>0, <JR C,.split_drop_clip>, <JR C,.split_clip_drop>
                            ENDIF
                        ENDIF
                        IFNDEF draw2DEX_viewport_no_x_max
                            IF dir_x>0 && clip_end || dir_x<0 && clip_begin
                                LD A,(x_max)
                                CP C
                                if_then_else dir_x>0, <JR C,.split_clip_drop>, <JR C,.split_drop_clip>
                            ENDIF
                        ENDIF

                        ; In all cases except clip/clip we can .restart this macro because we do a single clip with same <clip_begin, clip_end> arguments.
                        ; But in case of clip/clip we have to call .x1_lt__x2_ok/.x1_ok__x2_gt or .x1_gt__x2_ok/.x1_ok__x2_lt
                        ; to clip first and second line parts with <clip_begin=1, clip_end=0> and <clip_begin=0, clip_end=1> arguments respectively.

                        ; BC, DE       = xm, ym
                        ; BC', DE'     = x1, y1
                        ; (SP+2), (SP) = x2, y2

                        IF clip_begin && clip_end
.split_clip_clip            if_then_else dir_x>0, <CALL .x1_lt__x2_ok>, <CALL .x1_gt__x2_ok>
                            ; BC, DE       = new_x1, new_y1
                            ; BC', DE'     = xm, ym
                            ; (SP+2), (SP) = x2, y2
                            POP HL
                            EX DE,HL
                            EX (SP),HL
                            PUSH BC
                            LD B,H
                            LD C,L
                            ; BC', DE'     = xm, ym
                            ; BC, DE       = x2, y2
                            ; (SP), (SP+2) = new_x1, new_y1
                            if_then_else dir_x>0, <CALL .x1_ok__x2_gt>, <CALL .x1_ok__x2_lt>
                            ; BC', DE'     = new_x2, new_y2
                            ; (SP), (SP+2) = new_x1, new_y1
                            POP BC
                            POP DE
                            ; BC, DE   = new_x1, new_y1
                            ; BC', DE' = new_x2, new_y2
                            RET
                        ELSEIF clip_begin
.split_clip_keep            CALL .restart
                            ; BC, DE       = new_x1, new_y1
                            ; (SP+2), (SP) = x2, y2
                            EXX
                            POP DE
                            POP BC
                            _draw2DEX_clip_line_x_copy_xy
                            ; BC, DE   = new_x1, new_y1
                            ; BC', DE' = x2, y2
                            RET
                        ELSE
.split_keep_clip            EXX
                            POP HL
                            EX DE,HL
                            EX (SP),HL
                            PUSH BC
                            LD B,H
                            LD C,L
                            ; BC', DE'     = xm, ym
                            ; BC, DE       = x2, y2
                            ; (SP), (SP+2) = x1, y1
                            CALL .restart
                            ; BC', DE'     = new_x2, new_y2
                            ; (SP), (SP+2) = x1, y1
                            POP BC
                            POP DE
                            ; BC, DE   = x1, y1
                            ; BC', DE' = new_x2, new_y2
                            RET
                        ENDIF
.split_drop
                        IF clip_begin && clip_end
                            if_then_else dir_x>0, <JP P,.split_drop_clip>, <JP M,.split_drop_clip>
                        ENDIF
                        IF clip_end
.split_clip_drop            POP HL
                            POP HL
                            ; BC', DE' = x1, y1
                            ; BC, DE   = xm, ym
                            JP .restart
                        ENDIF
                        IF clip_begin
.split_drop_clip            EXX
                            POP DE
                            POP BC
                            ; BC', DE' = xm, ym
                            ; BC, DE   = x2, y2
                            JP .restart
                        ENDIF
                    ENDM

                    ; First we test both line points with _draw2DEX_clip_line_x_test, then clipping is done with _draw2DEX_clip_line_x if needed.
                    ; We have 9 cases:
                    ;                      |      x1 < x_min      | x_min <= x1 <= x_max |      x1 > x_max
                    ;----------------------+----------------------+----------------------+-------------------------------
                    ;                      |       outside        | clip_begin = 0       | clip_begin = 1
                    ;      x2 < x_min      |         RET          | clip_ end  = 1       | clip_end   = 1
                    ;                      |   (CF = 1, A = 0)    | dir_x = -1 (x1 > x2) | dir_x = -1 (x1 > x2)
                    ;----------------------+----------------------+----------------------+-------------------------------
                    ;                      | clip_begin = 1       |        inside        | clip_begin = 1
                    ; x_min <= x2 <= x_max | clip_end   = 0       |         RET          | clip_end   = 0
                    ;                      | dir_x = +1 (x1 < x2) |       (CF = 0)       | dir_x = -1 (x1 > x2)
                    ;----------------------+----------------------+----------------------+-------------------------------
                    ;                      | clip_begin = 1       | clip_begin = 0       |       outside
                    ;      x2 > x_max      | clip_end   = 1       | clip_ end  = 1       |         RET
                    ;                      | dir_x = +1 (x1 < x2) | dir_x = +1 (x1 < x2) |   (CF = 1, A <> 0)
                    ;----------------------+----------------------+----------------------+-------------------------------
                    ; Note that if x1 == x2 no clipping required, because whole line is completely inside or outside [x_min, x_max].

                    _draw2DEX_clip_line_x_test 0, .x1_lt, .x1_gt, .x1_out_0_FF
.x1_ok              EXX
                    _draw2DEX_clip_line_x_test 0, .x1_ok__x2_lt, .x1_ok__x2_gt, .x1_ok__x2_out_0_FF
.x1_ok__x2_ok       ; No clipping needed.
                    _draw2DEX_clip_line_x_copy_xy
                    RET

.x1_ok__x2_out_0_FF JP M,.x1_ok__x2_gt ; SF == 1 if x2 > 0
.x1_ok__x2_lt       _draw2DEX_clip_line_x 0, 1, -1
.x1_ok__x2_gt       _draw2DEX_clip_line_x 0, 1, +1

.x1_out_0_FF        JP M,.x1_gt ; SF == 1 if x1 > 0

.x1_lt              EXX
                    _draw2DEX_clip_line_x_test -1, 0, .x1_lt__x2_gt, .x1_lt__x2_out_0_FF
.x1_lt__x2_ok       _draw2DEX_clip_line_x 1, 0, +1
.x1_lt__x2_out_0_FF RET P ; SF == 0 if x2 < 0
.x1_lt__x2_gt       _draw2DEX_clip_line_x 1, 1, +1

.x1_gt              EXX
                    _draw2DEX_clip_line_x_test +1, .x1_gt__x2_lt, 0, .x1_gt__x2_out_0_FF
.x1_gt__x2_ok       _draw2DEX_clip_line_x 1, 0, -1
.x1_gt__x2_out_0_FF RET M ; SF == 1 if x2 > 0
.x1_gt__x2_lt       _draw2DEX_clip_line_x 1, 1, -1


set_viewport
; DE - x_max, x_min
; HL - y_max, y_min
; Preserves ALL except A.
                    LUA ALLPASS
                        borders = { E = "x_min", D = "x_max", L = "y_min", H = "y_max" }

                        for reg, border in pairs(borders) do
                            if sj.get_define("draw2DEX_viewport_no_"..border) == nil
                            then
                                _pc("LD A,"..reg)

                                addrs = draw2DEX_viewport_addrs[border]
                                for i = 1, #addrs do
                                    _pc("LD ("..addrs[i].."),A")
                                end

                                sj.insert_label("draw2DEX."..border, addrs[1])
                            end
                        end
                    ENDLUA

                    RET

                    ENDMODULE
                    milestone
