    MACRO box size_x,size_y,size_z,front,back,top,bottom,left,right
        BYTE 8
            BYTE -size_x/2,-size_y/2,-size_z/2
            BYTE +size_x/2,-size_y/2,-size_z/2
            BYTE -size_x/2,+size_y/2,-size_z/2
            BYTE +size_x/2,+size_y/2,-size_z/2
            BYTE -size_x/2,-size_y/2,+size_z/2
            BYTE +size_x/2,-size_y/2,+size_z/2
            BYTE -size_x/2,+size_y/2,+size_z/2
            BYTE +size_x/2,+size_y/2,+size_z/2
        BYTE 6
            BYTE 0,0,+100
            WORD front
            BYTE 4,6,7,5,-1

            BYTE 0,0,-100
            WORD back
            BYTE 0,1,3,2,-1

            BYTE 0,+100,0
            WORD top
            BYTE 6,2,3,7,-1

            BYTE 0,-100,0
            WORD bottom
            BYTE 4,5,1,0,-1

            BYTE -100,0,0
            WORD left
            BYTE 0,2,6,4,-1

            BYTE +100,0,0
            WORD right
            BYTE 5,7,3,1,-1
    ENDM
