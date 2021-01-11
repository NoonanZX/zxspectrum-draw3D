draw
.x_min              EQU 100
.x_max              EQU 200
.y_min              EQU 50
.y_max              EQU 150

                    LD DE,(.x_max<<8) + .x_min
                    LD HL,(.y_max<<8) + .y_min
                    CALL draw2DEX.set_viewport
                    CALL draw_viewport

                    LD HL,.points
                    LD BC,.points_count / 2
1                   PUSH BC

                    LD C,(HL)
                    INC HL
                    LD B,(HL)
                    INC HL
                    LD E,(HL)
                    INC HL
                    LD D,(HL)
                    INC HL

                    PUSH HL
                    EXX
                    POP HL

                    LD C,(HL)
                    INC HL
                    LD B,(HL)
                    INC HL
                    LD E,(HL)
                    INC HL
                    LD D,(HL)
                    INC HL

                    PUSH HL
                    CALL draw2DEX.draw_line
                    POP HL

                    POP BC
                    DEC BC
                    LD A,B
                    OR C
                    JP NZ,1B

                    DI
                    HALT

.points
	BYTE #EC,#FE,#AF,#01,#4F,#FF,#5A,#FF,#10,#01,#9D,#FE,#6F,#02,#7D,#01
	BYTE #01,#FF,#AA,#00,#7D,#01,#B4,#FE,#30,#00,#2B,#02,#80,#FF,#DA,#FF
	BYTE #66,#FF,#53,#FF,#C0,#00,#7D,#01,#DE,#FE,#EF,#FF,#65,#00,#F2,#00
	BYTE #4E,#01,#E8,#00,#8A,#01,#F6,#FE,#98,#00,#67,#02,#11,#02,#19,#00
	BYTE #DE,#FE,#5B,#00,#CD,#FE,#A1,#FF,#93,#FE,#ED,#00,#62,#01,#02,#02
	BYTE #21,#01,#DD,#01,#F8,#FE,#FD,#FE,#E0,#FE,#BD,#01,#76,#00,#69,#FF
	BYTE #91,#00,#2E,#FF,#7C,#02,#09,#02,#AC,#FE,#A6,#00,#25,#FF,#E4,#FE
	BYTE #A8,#01,#61,#FF,#80,#02,#A6,#FF,#BB,#FE,#D9,#01,#D8,#00,#76,#FF
	BYTE #AB,#01,#F7,#00,#E0,#FE,#49,#01,#1A,#FF,#E4,#00,#D1,#01,#98,#00
	BYTE #C0,#FF,#1C,#00,#87,#01,#A1,#00,#70,#02,#D3,#00,#69,#FF,#13,#00
	BYTE #EC,#00,#9B,#FF,#6C,#00,#EC,#01,#D4,#00,#5D,#01,#34,#02,#D7,#FE
	BYTE #E7,#01,#06,#01,#B9,#FE,#9E,#FF,#8B,#FF,#5B,#02,#1E,#02,#FC,#01
	BYTE #1C,#02,#28,#02,#39,#01,#91,#01,#CC,#00,#43,#00,#66,#00,#E8,#01
	BYTE #CE,#FE,#BA,#00,#FB,#FF,#34,#02,#2C,#01,#84,#00,#51,#01,#D4,#00
	BYTE #C8,#00,#44,#00,#5E,#00,#DB,#FE,#EF,#01,#5D,#FF,#6D,#02,#AD,#FF
	BYTE #60,#01,#BD,#FF,#D3,#FF,#2B,#01,#AF,#00,#B1,#FF,#23,#FF,#37,#02
	BYTE #B0,#00,#2B,#00,#E1,#00,#EF,#00,#55,#FF,#28,#FF,#42,#FF,#A9,#FF
	BYTE #AD,#FF,#C5,#FF,#8D,#FE,#2D,#00,#54,#02,#FD,#FE,#51,#01,#12,#01
	BYTE #47,#02,#19,#01,#C7,#FF,#25,#00,#B5,#FF,#AD,#FF,#BF,#FF,#10,#01
	BYTE #31,#01,#68,#00,#93,#00,#05,#FF,#AD,#FF,#D6,#FE,#32,#01,#FC,#FF
	BYTE #96,#01,#9D,#01,#0B,#01,#14,#01,#E9,#01,#19,#02,#CC,#01,#B3,#01
	BYTE #48,#01,#2D,#01,#B4,#FE,#A8,#01,#84,#FF,#CD,#00,#1C,#02,#20,#00
	BYTE #9B,#01,#00,#00,#B3,#FF,#37,#01,#FE,#01,#29,#FF,#4C,#00,#5F,#FF
	BYTE #20,#02,#3A,#01,#C8,#00,#08,#00,#E4,#FE,#FE,#FF,#8F,#FE,#26,#FF
	BYTE #50,#00,#44,#02,#F6,#01,#21,#00,#A2,#FE,#46,#01,#84,#00,#B5,#01
	BYTE #BD,#01,#CA,#FF,#AD,#FE,#6E,#FF,#4E,#FF,#82,#FE,#BE,#FF,#7E,#00
	BYTE #B2,#00,#73,#00,#5E,#01,#5F,#00,#7E,#FF,#E9,#00,#54,#FF,#08,#FF
	BYTE #91,#01,#5D,#02,#58,#00,#F6,#00,#22,#00,#8D,#01,#F1,#01,#4B,#02
	BYTE #86,#FF,#49,#01,#4B,#01,#38,#00,#B0,#FE,#4D,#FF,#72,#FF,#F6,#FE
	BYTE #07,#01,#79,#FF,#09,#FF,#21,#02,#DA,#00,#3B,#02,#54,#00,#7F,#FF
	BYTE #A2,#FF,#74,#02,#D3,#01,#92,#FF,#7C,#FF,#C2,#01,#6B,#01,#4B,#FF
	BYTE #D2,#00,#72,#02,#C4,#00,#E9,#FE,#71,#01,#25,#00,#A7,#FF,#5F,#FF
	BYTE #C6,#FF,#7B,#00,#54,#02,#3A,#FF,#56,#02,#0A,#01,#63,#01,#3F,#FF
	BYTE #E9,#FF,#37,#01,#99,#01,#76,#FF,#4D,#FF,#15,#02,#38,#01,#14,#FF
	BYTE #D1,#FE,#49,#00,#0D,#00,#37,#01,#0E,#01,#53,#01,#DA,#01,#7D,#02
	BYTE #93,#FE,#45,#FF,#C0,#00,#A6,#FE,#24,#00,#00,#02,#F7,#01,#E7,#00
	BYTE #EC,#01,#D6,#FF,#3D,#FF,#3D,#FF,#1C,#FF,#6E,#FF,#C4,#01,#9A,#01
	BYTE #80,#01,#9C,#00,#98,#00,#A4,#01,#F7,#01,#BA,#01,#93,#00,#A8,#00
	BYTE #05,#FF,#C4,#FF,#1B,#00,#32,#01,#B4,#00,#96,#FF,#B5,#FF,#08,#01
	BYTE #CF,#FE,#B4,#00,#17,#02,#19,#02,#35,#00,#A2,#FE,#B1,#00,#5B,#00
	BYTE #18,#00,#90,#00,#25,#00,#ED,#01,#A2,#01,#18,#00,#83,#FE,#3F,#00
	BYTE #5D,#FF,#70,#01,#26,#00,#AE,#FE,#2C,#01,#11,#02,#A1,#FE,#B4,#01
	BYTE #B5,#00,#3B,#00,#3F,#00,#F4,#00,#A6,#FF,#F0,#FF,#23,#02,#08,#02
	BYTE #6C,#00,#B3,#00,#EC,#00,#E3,#01,#CE,#01,#5C,#02,#21,#02,#C6,#00
	BYTE #AC,#FE,#75,#01,#8A,#FF,#61,#02,#1D,#02,#10,#02,#FE,#FF,#38,#02
	BYTE #27,#02,#C7,#FE,#D2,#01,#D5,#00,#99,#01,#72,#02,#52,#01,#F9,#FE
	BYTE #32,#01,#66,#00,#6F,#02,#B5,#FF,#97,#01,#F9,#FE,#EB,#FF,#31,#01
	BYTE #6C,#02,#8C,#01,#76,#00,#04,#02,#BD,#FE,#B2,#FF,#D5,#00,#11,#02
	BYTE #61,#01,#84,#01,#AA,#01,#E9,#01,#65,#02,#68,#FF,#E3,#FF,#3C,#02
	BYTE #AC,#01,#15,#01,#2D,#01,#D8,#FF,#31,#FF,#FA,#00,#C7,#FF,#BA,#FF
	BYTE #58,#02,#97,#FF,#B8,#00,#DF,#00,#19,#02,#DE,#00,#11,#00,#C0,#FF
	BYTE #36,#02,#99,#FE,#B5,#FE,#B4,#FE,#E6,#FE,#31,#00,#09,#FF,#19,#01
	BYTE #32,#FF,#D0,#FF,#58,#FF,#9F,#01,#33,#02,#79,#00,#AC,#FE,#9C,#01
	BYTE #2E,#01,#A7,#00,#08,#01,#D7,#00,#39,#02,#F9,#00,#CC,#00,#81,#FF
	BYTE #FC,#00,#5B,#FF,#DF,#FE,#59,#02,#C2,#FE,#E9,#FE,#81,#FE,#8C,#FE
	BYTE #31,#00,#F9,#00,#AD,#00,#96,#00,#37,#01,#96,#01,#98,#FE,#59,#00
	BYTE #8A,#FF,#D4,#FF,#20,#01,#40,#02,#82,#FF,#DA,#00,#CD,#FF,#4B,#FF
	BYTE #A6,#FE,#76,#00,#4D,#00,#FD,#01,#45,#FF,#A7,#01,#60,#FF,#CD,#00
	BYTE #66,#00,#D1,#01,#3B,#01,#F8,#FF,#1C,#02,#43,#FF,#3B,#00,#1C,#FF
	BYTE #4D,#00,#6D,#01,#E7,#FE,#25,#FF,#12,#02,#F2,#FE,#C8,#FF,#B2,#FF
	BYTE #40,#01,#94,#FE,#D4,#FE,#03,#00,#D2,#FE,#45,#01,#06,#02,#EA,#FF
	BYTE #10,#FF,#BB,#00,#5A,#02,#A1,#FE,#1B,#02,#F7,#00,#70,#00,#49,#02
	BYTE #F2,#01,#71,#01,#02,#02,#BE,#01,#C9,#00,#0F,#02,#E0,#01,#5D,#00
	BYTE #37,#FF,#3B,#01,#A7,#FE,#83,#FF,#34,#02,#03,#01,#D9,#01,#A8,#01
	BYTE #3D,#00,#0F,#FF,#95,#01,#9B,#FF,#30,#FF,#C7,#FE,#18,#FF,#06,#FF
	BYTE #91,#00,#46,#01,#D0,#FF,#EF,#01,#16,#FF,#C4,#01,#0E,#00,#60,#00
	BYTE #B6,#FE,#96,#FF,#5C,#00,#76,#01,#EC,#FE,#71,#01,#D2,#FE,#79,#02
	BYTE #28,#FF,#F3,#FF,#B0,#FF,#6B,#01,#21,#01,#E6,#01,#7D,#FF,#70,#02
	BYTE #7C,#00,#35,#00,#24,#01,#1F,#02,#4D,#FF,#FB,#FF,#C3,#FF,#1C,#02
	BYTE #F4,#FF,#6D,#FF,#BE,#FE,#DC,#FE,#59,#FF,#DB,#FF,#F3,#00,#24,#00
	BYTE #EB,#01,#AC,#FF,#F7,#00,#E5,#FE,#96,#00,#6C,#FF,#FA,#FE,#88,#00
	BYTE #5B,#00,#2F,#00,#6E,#02,#5D,#00,#98,#01,#DC,#FF,#A8,#00,#91,#00
	BYTE #9A,#00,#0B,#01,#62,#00,#C0,#01,#FF,#00,#79,#FF,#02,#02,#98,#01
	BYTE #9A,#FE,#5C,#00,#9E,#FE,#AB,#FF,#18,#02,#4D,#01,#6F,#00,#9F,#01
	BYTE #6D,#02,#D6,#FF,#CA,#FE,#ED,#00,#F7,#01,#FF,#01,#27,#02,#8A,#00
	BYTE #BD,#FE,#B7,#01,#09,#00,#CE,#00,#E5,#FE,#4B,#FF,#59,#00,#14,#01
	BYTE #D5,#01,#53,#00,#37,#01,#BF,#01,#57,#01,#9C,#FE,#8E,#00,#40,#FF
	BYTE #E2,#FE,#95,#00,#61,#00,#C9,#00,#83,#FF,#7B,#FF,#9D,#FE,#E5,#01
	BYTE #41,#00,#9A,#FF,#2E,#00,#95,#00,#88,#FE,#C9,#00,#C0,#FF,#64,#FF
	BYTE #83,#00,#0A,#01,#00,#01,#86,#FF,#B8,#FE,#7C,#FF,#E9,#01,#F9,#01
	BYTE #8D,#00,#A6,#01,#FD,#01,#19,#00,#11,#00,#24,#FF,#1C,#00,#78,#FF
	BYTE #51,#01,#EF,#01,#B3,#00,#B2,#FE,#FA,#01,#2A,#00,#10,#01,#8B,#FE
	BYTE #E9,#01,#0C,#FF,#85,#FE,#B7,#FF,#1D,#00,#58,#01,#B7,#01,#8F,#01
	BYTE #F5,#00,#4E,#00,#16,#00,#A4,#00,#D5,#00,#13,#02,#C5,#FE,#29,#02
	BYTE #9A,#00,#DD,#00,#D0,#FE,#24,#FF,#D3,#FE,#15,#00,#D0,#FF,#D5,#01
	BYTE #55,#00,#59,#FF,#0D,#01,#0D,#01,#DE,#00,#5E,#FF,#7C,#02,#C3,#FE
	BYTE #CF,#00,#79,#00,#53,#00,#EE,#00,#BD,#01,#F8,#FF,#AA,#FF,#5C,#FF
	BYTE #E4,#FE,#C2,#FE,#94,#FE,#19,#01,#7B,#01,#21,#FF,#2B,#FF,#18,#02
	BYTE #C8,#FF,#5E,#01,#D0,#01,#14,#01,#33,#02,#21,#00,#66,#02,#93,#FF
	BYTE #02,#01,#49,#FF,#17,#01,#0C,#02,#1B,#02,#7F,#01,#74,#FF,#CD,#00
	BYTE #21,#02,#A4,#00,#65,#01,#B4,#01,#5A,#02,#C8,#00,#9A,#FF,#1C,#00
	BYTE #FA,#FF,#6F,#00,#71,#02,#93,#01,#B0,#FF,#A7,#01,#E5,#01,#F9,#FE
	BYTE #00,#01,#2A,#01,#B0,#FE,#D4,#00,#85,#FE,#57,#FF,#C6,#00,#A3,#01
	BYTE #F7,#FE,#73,#02,#6D,#00,#5F,#00,#81,#01,#C5,#00,#66,#01,#7D,#02
	BYTE #5A,#FF,#21,#01,#6A,#00,#1A,#01,#58,#FF,#95,#FE,#36,#FF,#4E,#01
	BYTE #5B,#FF,#63,#01,#1C,#00,#73,#02,#AA,#00,#BA,#FF,#8C,#01,#77,#01
	BYTE #F7,#FF,#12,#FF,#A9,#FF,#80,#FE,#A9,#FF,#6E,#01,#D3,#FF,#86,#01
	BYTE #87,#FF,#3D,#FF,#34,#02,#49,#00,#15,#01,#35,#02,#FF,#00,#AE,#00
	BYTE #A6,#00,#22,#01,#82,#00,#5D,#01,#4E,#FF,#6F,#02,#D0,#01,#01,#00
	BYTE #5E,#01,#B7,#00,#AF,#FF,#8B,#00,#66,#01,#BF,#FE,#F7,#00,#BB,#FF
	BYTE #20,#FF,#72,#00,#FB,#FE,#EC,#FE,#75,#02,#81,#01,#DF,#01,#68,#00
	BYTE #64,#02,#86,#FF,#8D,#00,#BE,#FE,#C8,#01,#5D,#02,#58,#FF,#99,#FF
	BYTE #62,#01,#54,#02,#F5,#FE,#6E,#02,#45,#FF,#EC,#FE,#AC,#01,#7F,#01
	BYTE #47,#01,#9F,#FF,#5E,#02,#12,#01,#38,#02,#26,#01,#95,#00,#3C,#FF
	BYTE #F4,#FF,#9F,#FF,#91,#FF,#D8,#FF,#8A,#FF,#B3,#FF,#B0,#FF,#9F,#FE
	BYTE #82,#FE,#DB,#FF,#33,#01,#E2,#FF,#F5,#FE,#12,#01,#AA,#FF,#58,#00
	BYTE #28,#00,#8C,#00,#B6,#FE,#8D,#01,#FC,#FE,#AB,#01,#AD,#FF,#0C,#01
	BYTE #4D,#01,#97,#01,#AE,#00,#3C,#01,#A0,#FE,#1F,#FF,#64,#02,#42,#00
	BYTE #43,#01,#A5,#01,#DE,#01,#3F,#FF,#34,#00,#FB,#00,#D9,#FF,#07,#01
	BYTE #2C,#FF,#6F,#01,#AE,#01,#CD,#FE,#77,#FF,#B7,#FF,#00,#00,#6D,#02
	BYTE #DD,#00,#6E,#02,#AC,#00,#2E,#01,#5E,#01,#13,#02,#69,#00,#E6,#FF
	BYTE #03,#02,#70,#FF,#AE,#FE,#83,#00,#95,#01,#EB,#00,#98,#FE,#1F,#FF
	BYTE #F0,#00,#E6,#00,#F7,#FE,#7E,#02,#42,#00,#BE,#00,#51,#FF,#F0,#FE
	BYTE #93,#FE,#22,#02,#A6,#FF,#4F,#FF,#3B,#00,#DA,#FE,#61,#01,#D4,#00
	BYTE #31,#FF,#11,#FF,#F0,#01,#CF,#00,#51,#01,#7C,#01,#2A,#FF,#74,#00
	BYTE #1D,#01,#5E,#01,#CB,#FE,#CA,#FE,#2A,#02,#7C,#02,#3D,#02,#37,#00
	BYTE #AB,#01,#64,#02,#38,#FF,#21,#00,#C9,#FE,#C6,#FE,#8F,#01,#4E,#01
	BYTE #F9,#FF,#73,#02,#8F,#01,#C6,#01,#D6,#01,#33,#01,#BB,#00,#94,#FE
	BYTE #07,#00,#67,#00,#E7,#00,#10,#01,#21,#02,#AD,#FF,#04,#00,#D4,#00
	BYTE #49,#02,#F8,#01,#03,#FF,#5A,#00,#09,#00,#C5,#01,#62,#01,#43,#FF
	BYTE #F7,#FE,#EF,#01,#C5,#FE,#DC,#FE,#09,#01,#42,#00,#A1,#00,#40,#01
	BYTE #DA,#00,#D6,#FF,#A3,#00,#33,#02,#08,#02,#44,#01,#96,#01,#FB,#FE
	BYTE #D2,#FF,#17,#00,#BD,#01,#96,#FF,#77,#02,#FE,#FF,#96,#FE,#AC,#FF
	BYTE #22,#FF,#9F,#00,#46,#FF,#4A,#00,#C0,#FE,#F6,#00,#23,#02,#3A,#02
	BYTE #2B,#FF,#64,#00,#D4,#00,#A2,#00,#99,#00,#ED,#FF,#55,#FF,#AE,#FE
	BYTE #A0,#FE,#37,#02,#6F,#00,#64,#02,#E6,#01,#66,#01,#14,#02,#B8,#FE
	BYTE #45,#02,#B7,#01,#F6,#00,#C3,#00,#A5,#01,#B8,#FF,#92,#01,#5C,#01
	BYTE #09,#01,#CF,#FE,#03,#FF,#50,#00,#F5,#FF,#9A,#00,#D9,#00,#4E,#FF
	BYTE #F1,#01,#40,#FF,#9F,#FF,#9F,#01,#6E,#01,#92,#FF,#50,#FF,#05,#FF
	BYTE #A4,#FE,#0D,#00,#9D,#00,#BB,#00,#F2,#FF,#21,#01,#03,#00,#A6,#01
	BYTE #D8,#FF,#A8,#01,#C6,#00,#E0,#00,#17,#00,#8A,#FF,#6C,#02,#BD,#FE
	BYTE #20,#02,#C7,#FF,#2D,#02,#44,#02,#B7,#00,#38,#00,#5A,#01,#B9,#FF
	BYTE #4B,#FF,#07,#02,#B1,#00,#7E,#00,#50,#00,#52,#FF,#5D,#02,#CF,#FE
	BYTE #E9,#00,#9C,#00,#E9,#01,#83,#01,#81,#FE,#69,#FF,#05,#00,#74,#01
	BYTE #85,#FF,#B1,#01,#37,#02,#AD,#FF,#D3,#00,#4B,#02,#E2,#FE,#34,#01
	BYTE #96,#FF,#CF,#FF,#EA,#FF,#B6,#FF,#58,#FF,#C9,#FE,#45,#01,#DC,#00
	BYTE #D2,#FE,#47,#02,#03,#01,#03,#02,#F8,#FE,#86,#FE,#31,#FF,#96,#FF
	BYTE #62,#FF,#EB,#FF,#12,#00,#35,#00,#70,#00,#DC,#FF,#C8,#FE,#44,#00
	BYTE #EB,#01,#0C,#FF,#27,#02,#24,#00,#D9,#FF,#5E,#00,#2F,#FF,#23,#02
	BYTE #95,#FF,#59,#02,#D1,#FE,#CE,#00,#78,#00,#B4,#FE,#4C,#02,#B1,#FE
	BYTE #C7,#01,#77,#00,#EE,#00,#4E,#00,#22,#02,#6E,#01,#0D,#02,#4F,#FF
	BYTE #D9,#01,#4C,#01,#C2,#00,#DD,#FF,#3F,#00,#BB,#FE,#C4,#01,#40,#00
	BYTE #E7,#FF,#02,#00,#4B,#02,#EC,#FE,#42,#00,#E9,#FE,#C7,#00,#08,#00
	BYTE #C3,#00,#FC,#00,#B4,#00,#CA,#01,#A9,#01,#3F,#00,#B4,#00,#14,#FF
	BYTE #D3,#01,#22,#00,#03,#02,#36,#01,#BA,#FF,#75,#02,#38,#02,#A4,#FE
	BYTE #8C,#FF,#8C,#FE,#EF,#FF,#D1,#00,#C0,#00,#89,#FF,#FB,#00,#A4,#FE
	BYTE #BB,#01,#6E,#FF,#E8,#FF,#00,#FF,#69,#02,#3D,#FF,#17,#00,#9A,#FE
	BYTE #8B,#00,#71,#00,#92,#FF,#B9,#FE,#81,#00,#23,#01,#21,#01,#E1,#FF
	BYTE #A2,#01,#62,#02,#20,#FF,#74,#FF,#EA,#FE,#A4,#FE,#FF,#FF,#D4,#00
	BYTE #85,#01,#F5,#01,#4F,#FF,#7C,#02,#F9,#01,#1C,#02,#63,#00,#7F,#00
	BYTE #5B,#FF,#F3,#FF,#B7,#FF,#0F,#01,#35,#01,#AA,#00,#AA,#01,#02,#01
	BYTE #58,#FF,#CF,#00,#70,#FF,#FE,#01,#90,#FE,#C5,#01,#2B,#01,#BF,#FE
	BYTE #06,#01,#65,#01,#38,#00,#E3,#00,#24,#02,#D0,#00,#5E,#02,#86,#01
	BYTE #67,#02,#DC,#FF,#DB,#FE,#DF,#00,#84,#01,#51,#02,#42,#01,#7A,#02
	BYTE #89,#FF,#CC,#01,#72,#02,#BD,#FF,#AC,#00,#E4,#01,#AE,#FE,#0E,#01
	BYTE #10,#FF,#BD,#01,#44,#01,#24,#01,#6E,#01,#FB,#FE,#6E,#00,#4E,#01
	BYTE #BC,#00,#96,#FE,#56,#FF,#4B,#01,#8C,#01,#CD,#FE,#03,#FF,#C9,#01
	BYTE #6F,#00,#BE,#FE,#B1,#00,#D1,#01,#D9,#00,#F6,#FF,#91,#01,#8C,#00
	BYTE #74,#FF,#34,#00,#F2,#01,#E7,#FF,#43,#02,#7D,#01,#82,#FE,#29,#FF
	BYTE #30,#00,#B8,#00,#A0,#01,#67,#FF,#AC,#FE,#3C,#00,#96,#01,#C3,#01
	BYTE #C8,#FE,#99,#FF,#EA,#FF,#8B,#01,#65,#02,#06,#01,#4A,#01,#6A,#FF
	BYTE #2D,#FF,#25,#01,#4E,#01,#09,#00,#A8,#01,#A2,#00,#AD,#FE,#7F,#FF
	BYTE #37,#02,#C7,#01,#76,#00,#01,#02,#86,#00,#C4,#FF,#3D,#FF,#04,#00
	BYTE #5E,#01,#83,#FE,#31,#01,#E6,#FE,#84,#FF,#66,#02,#BA,#00,#38,#02
	BYTE #24,#FF,#86,#FF,#02,#02,#5E,#02,#40,#01,#12,#FF,#E1,#FF,#F5,#01
	BYTE #2E,#FF,#6E,#00,#97,#FE,#66,#02,#C9,#FE,#67,#02,#18,#00,#7E,#FF
	BYTE #1A,#01,#29,#FF,#F8,#00,#30,#FF,#0B,#01,#0D,#01,#9D,#FF,#30,#02
	BYTE #5F,#01,#1E,#00,#3A,#00,#6D,#02,#1A,#00,#FF,#FE,#E7,#01,#B7,#FF
	BYTE #6A,#02,#E7,#00,#D8,#FE,#57,#02,#82,#01,#B7,#01,#6E,#FF,#50,#02
	BYTE #3B,#00,#0B,#01,#11,#01,#01,#00,#F8,#01,#D6,#01,#29,#02,#45,#FF
	BYTE #9C,#FE,#80,#01,#28,#00,#07,#02,#2C,#FF,#3C,#FF,#82,#01,#24,#01
	BYTE #74,#FF,#09,#00,#52,#01,#89,#00,#E9,#00,#9F,#00,#5D,#00,#0B,#02
	BYTE #00,#FF,#0F,#01,#4D,#01,#34,#00,#B1,#FF,#0A,#02,#56,#00,#ED,#01
	BYTE #04,#02,#02,#02,#B1,#01,#CF,#01,#4B,#00,#85,#FF,#EB,#01,#68,#00
	BYTE #F4,#FE,#0E,#00,#CC,#FE,#74,#02,#06,#02,#9C,#FE,#13,#02,#28,#02
	BYTE #18,#02,#11,#02,#9C,#FF,#80,#FE,#00,#FF,#BD,#FE,#F8,#00,#0C,#01
	BYTE #FD,#FF,#ED,#01,#14,#00,#AB,#00,#72,#00,#24,#01,#DD,#00,#82,#FF
	BYTE #83,#FE,#BA,#FE,#0B,#02,#06,#00,#75,#FF,#4B,#02,#54,#00,#73,#FF
	BYTE #F7,#FF,#11,#01,#C1,#FE,#45,#00,#18,#FF,#87,#00,#2F,#01,#B0,#00
	BYTE #ED,#01,#3B,#FF,#69,#00,#6B,#00,#87,#FF,#01,#02,#AD,#00,#AC,#01
	BYTE #8D,#FE,#53,#02,#2F,#FF,#70,#02,#DA,#FE,#6A,#01,#08,#00,#8B,#00
	BYTE #FF,#00,#0F,#02,#EB,#FE,#39,#02,#18,#01,#06,#00,#50,#02,#23,#02
	BYTE #2E,#02,#32,#00,#D6,#00,#61,#00,#B1,#FE,#3B,#01,#80,#01,#B8,#FE
	BYTE #C1,#00,#08,#FF,#5E,#00,#F8,#01,#F4,#FE,#AD,#01,#A5,#00,#AC,#00
	BYTE #F9,#FE,#E8,#00,#90,#FE,#F3,#01,#47,#02,#CB,#00,#F6,#00,#F1,#FE
	BYTE #2F,#FF,#76,#02,#D3,#FE,#DF,#01,#B9,#FE,#F9,#FE,#69,#02,#64,#01
	BYTE #3F,#00,#40,#02,#9B,#01,#58,#01,#F7,#01,#A3,#FE,#D1,#00,#15,#02
	BYTE #20,#FF,#94,#FF,#A7,#01,#30,#01,#D5,#01,#88,#00,#99,#FF,#03,#02
	BYTE #69,#FF,#F0,#FF,#83,#FE,#18,#00,#C5,#01,#D3,#FF,#8D,#FF,#66,#01
	BYTE #40,#FF,#21,#00,#6C,#02,#D4,#00,#4F,#02,#1D,#00,#EC,#FF,#51,#02
	BYTE #C1,#01,#3D,#00,#0A,#02,#B1,#FF,#FB,#00,#C3,#00,#EC,#FE,#61,#FF
	BYTE #7E,#02,#AB,#FF,#F7,#01,#F2,#FF,#D3,#00,#2C,#FF,#6A,#00,#BD,#00
	BYTE #AB,#FF,#5F,#02,#6C,#FF,#BF,#00,#A0,#FE,#D5,#00,#7D,#00,#9C,#01
	BYTE #AD,#01,#6C,#FF,#17,#01,#B9,#FE,#E2,#01,#64,#02,#67,#FF,#3D,#FF
	BYTE #EA,#FF,#55,#00,#DB,#00,#7C,#FF,#98,#FF,#60,#02,#81,#FE,#BA,#00
	BYTE #E6,#FF,#4B,#02,#19,#01,#C0,#FF,#05,#FF,#6D,#01,#9F,#00,#DF,#00
	BYTE #0E,#FF,#B3,#FE,#86,#FF,#EC,#FE,#EA,#01,#42,#FF,#E8,#FE,#79,#01
	BYTE #8C,#01,#1C,#FF,#71,#02,#25,#01,#80,#02,#8A,#FF,#04,#00,#69,#02
	BYTE #C0,#01,#E4,#FF,#F9,#00,#64,#00,#98,#FF,#51,#FF,#44,#00,#11,#02
	BYTE #4C,#00,#D4,#FE,#2B,#02,#DE,#00,#34,#FF,#8D,#00,#21,#FF,#08,#01
	BYTE #F3,#00,#21,#00,#61,#00,#4E,#01,#28,#01,#90,#00,#CA,#FE,#AF,#00
	BYTE #01,#02,#5C,#01,#8A,#FE,#C9,#01,#7A,#FF,#55,#01,#EB,#FE,#83,#FF
	BYTE #75,#01,#05,#00,#0B,#FF,#8A,#FF,#B9,#01,#D9,#FE,#64,#00,#E7,#01
	BYTE #CE,#FE,#C4,#00,#B7,#FE,#D9,#00,#DC,#01,#73,#01,#E3,#FE,#36,#00
	BYTE #06,#02,#E2,#FF,#AD,#01,#5E,#02,#76,#00,#92,#FE,#6B,#00,#A1,#FE
	BYTE #BB,#FE,#E3,#FF,#E2,#FE,#26,#01,#3F,#FF,#B4,#FE,#00,#FF,#BA,#00
	BYTE #15,#FF,#2D,#01,#70,#02,#A0,#01,#E3,#00,#6C,#00,#49,#00,#12,#00
	BYTE #A7,#FF,#EF,#FE,#76,#FF,#CB,#FE,#B3,#00,#0B,#02,#7F,#02,#DB,#FF
	BYTE #81,#FE,#5A,#FF,#4B,#FF,#5B,#01,#1B,#00,#EA,#01,#E1,#01,#BA,#FE
	BYTE #1E,#02,#09,#01,#15,#02,#16,#FF,#3A,#02,#B6,#00,#04,#02,#C0,#00
	BYTE #C5,#00,#EE,#FE,#D2,#FE,#58,#02,#2F,#02,#F7,#01,#DC,#FE,#C3,#01
	BYTE #B5,#00,#8B,#FE,#0E,#01,#84,#00,#DC,#FF,#C0,#00,#20,#02,#76,#01
	BYTE #7A,#02,#F2,#01,#0E,#00,#40,#FF,#AA,#FF,#1C,#00,#25,#FF,#C5,#01
	BYTE #52,#02,#F2,#FE,#45,#FF,#AF,#FE,#DE,#01,#96,#FF,#3B,#01,#88,#FF
	BYTE #42,#FF,#F1,#FF,#30,#00,#BD,#01,#D5,#01,#B2,#01,#88,#FF,#E8,#00
	BYTE #2D,#01,#2B,#00,#17,#01,#6D,#FF,#53,#00,#D2,#FF,#B5,#FE,#35,#01
	BYTE #5E,#02,#45,#01,#3F,#02,#35,#01,#97,#00,#D1,#01,#D9,#FE,#13,#02
	BYTE #01,#02,#E4,#FF,#C9,#FF,#18,#02,#DE,#FF,#E4,#01,#84,#01,#4B,#02
	BYTE #45,#00,#63,#00,#14,#FF,#62,#00,#DD,#FE,#AD,#FE,#E1,#FE,#1E,#02
	BYTE #48,#00,#EE,#FE,#AC,#01,#EB,#FF,#26,#02,#0A,#00,#2C,#02,#E2,#00
	BYTE #D8,#00,#81,#FE,#7A,#01,#E7,#01,#4C,#02,#61,#01,#75,#01,#73,#00
	BYTE #A6,#FE,#20,#FF,#A7,#FF,#BD,#00,#37,#00,#53,#02,#4B,#02,#DA,#FF
	BYTE #82,#00,#E4,#FF,#84,#01,#D2,#00,#86,#FE,#5A,#01,#5D,#00,#A4,#FE
	BYTE #BF,#00,#23,#00,#39,#FF,#43,#02,#0E,#00,#4B,#00,#0D,#02,#CC,#FE
	BYTE #E0,#01,#68,#01,#74,#FF,#D4,#00,#91,#FF,#0C,#FF,#D3,#01,#E6,#FE
	BYTE #23,#01,#B4,#00,#CD,#00,#F9,#FF,#3F,#01,#AF,#FF,#20,#FF,#B7,#FE
	BYTE #51,#FF,#AE,#FF,#B3,#FF,#3E,#02,#DE,#00,#81,#01,#EF,#01,#A8,#00
	BYTE #DB,#01,#EC,#FE,#72,#00,#F6,#01,#41,#01,#51,#02,#5C,#FF,#2D,#01
	BYTE #60,#01,#D5,#00,#FB,#00,#17,#01,#65,#02,#83,#00,#E2,#FF,#74,#00
	BYTE #13,#00,#0F,#01,#65,#01,#47,#FF,#CB,#00,#3E,#00,#8F,#00,#5D,#00
	BYTE #EE,#00,#72,#FF,#C4,#01,#BF,#FF,#3E,#01,#E9,#01,#13,#00,#9A,#FF
	BYTE #C5,#01,#2F,#01,#02,#02,#15,#00,#C5,#00,#9A,#00,#A0,#FE,#62,#02
	BYTE #32,#FF,#0B,#00,#4E,#00,#EE,#00,#B0,#01,#8E,#FF,#80,#FF,#CF,#FE
	BYTE #E1,#00,#7C,#02,#6D,#02,#67,#00,#0D,#02,#45,#02,#DD,#FE,#76,#02
	BYTE #33,#02,#E8,#00,#D4,#01,#06,#01,#21,#02,#A2,#01,#98,#FE,#67,#00
	BYTE #7D,#01,#3A,#00,#40,#02,#EC,#FF,#E7,#00,#6C,#01,#62,#00,#DF,#FF
	BYTE #7B,#FF,#BC,#FE,#AA,#FE,#49,#02,#91,#01,#7B,#00,#45,#FF,#51,#FF
	BYTE #4C,#02,#1C,#FF,#08,#02,#F4,#01,#30,#01,#5C,#02,#8B,#01,#4A,#FF
	BYTE #39,#01,#D3,#01,#3E,#00,#EF,#FF,#92,#01,#4D,#00,#02,#02,#35,#02
	BYTE #2C,#01,#49,#FF,#1A,#00,#D0,#01,#6C,#00,#A2,#FF,#71,#FF,#BC,#FE
	BYTE #5A,#02,#3B,#01,#2F,#01,#11,#00,#AA,#01,#B3,#01,#BF,#FE,#92,#00
	BYTE #8F,#FE,#07,#FF,#7F,#FF,#CF,#00,#B0,#01,#B9,#00,#B6,#FE,#F2,#FE
	BYTE #63,#FF,#F5,#01,#36,#FF,#DE,#01,#93,#FF,#A9,#FE,#5E,#01,#CF,#FF
	BYTE #20,#00,#D4,#01,#EC,#00,#BD,#01,#49,#02,#A6,#00,#D2,#FE,#B3,#00
	BYTE #26,#02,#1E,#00,#88,#FE,#41,#02,#C7,#FF,#D3,#FF,#FA,#FE,#77,#FF
	BYTE #1F,#00,#05,#FF,#A2,#FE,#49,#02,#C5,#01,#B7,#01,#DE,#01,#68,#02
	BYTE #EC,#01,#2D,#FF,#E4,#FE,#28,#00,#9B,#01,#87,#01,#2E,#FF,#C4,#01
	BYTE #AF,#00,#F9,#FF,#36,#FF,#4A,#02,#CD,#01,#A6,#FE,#5A,#FF,#81,#00
	BYTE #98,#FE,#A3,#FE,#FB,#00,#8D,#01,#18,#00,#BE,#FE,#D1,#01,#1D,#FF
	BYTE #CF,#01,#35,#FF,#A4,#FE,#8D,#00,#62,#FF,#86,#00,#86,#00,#FA,#FF
	BYTE #BF,#FF,#53,#FF,#D5,#00,#40,#00,#EF,#FE,#9D,#01,#22,#00,#79,#00
	BYTE #4F,#FF,#B6,#01,#C6,#00,#7E,#FF,#16,#FF,#22,#02,#94,#FF,#0C,#FF
	BYTE #62,#02,#F5,#00,#3A,#01,#DE,#FE,#FC,#00,#17,#FF,#36,#FF,#6D,#02
	BYTE #78,#02,#9E,#01,#38,#FF,#76,#01,#B6,#00,#19,#00,#99,#FF,#A1,#01
	BYTE #53,#02,#1A,#FF,#68,#01,#DC,#FF,#0D,#FF,#E2,#01,#84,#00,#40,#02
	BYTE #1A,#00,#C9,#FE,#81,#FE,#70,#02,#9D,#FE,#7E,#02,#44,#01,#14,#FF
.points_count       EQU ($ - .points) / 4


draw_viewport
                    LD D,draw.x_min
                    LD E,draw.y_min
                    LD H,draw.x_max
                    CALL draw2D.draw_horizontal_line

                    LD D,draw.x_min
                    LD E,draw.y_max
                    LD H,draw.x_max
                    CALL draw2D.draw_horizontal_line

                    LD D,draw.x_min
                    LD E,draw.y_min
                    LD L,draw.y_max
                    CALL draw2D.draw_vertical_line

                    LD D,draw.x_max
                    LD E,draw.y_min
                    LD L,draw.y_max
                    JP draw2D.draw_vertical_line
