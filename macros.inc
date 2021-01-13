                    MACRO sAto16 reg_hi, reg_lo
                        LD reg_lo,A
                        ADD A
                        SBC A
                        LD reg_hi,A
                    ENDM


                    MACRO if_then pred, cmd
                        IF pred
                            cmd
                        ENDIF
                    ENDM


                    MACRO if_then_else pred, cmd1, cmd2
                        IF pred
                            cmd1
                        ELSE
                            cmd2
                        ENDIF
                    ENDM


                    MACRO milestone
                        LUA
                            addr = sj.current_address
                            file = sj.get_define("__FILE__")
                            line = sj.get_define("__LINE__")

                            if milestone_last ~= nil and milestone_last.addr ~= addr then
                                if file ~= milestone_last.file
                                then
                                    msg = milestone_last.file..'('..milestone_last.line..") - "..file..'('..line..") "
                                else
                                    msg = file..'('..milestone_last.line..'-'..line..") "
                                end

                                msg = msg..string.rep('-', math.max(30 - string.len(msg), 1))..' '..(addr - milestone_last.addr).." bytes"

                                print(msg)
                            end

                            milestone_last =
                            {
                                addr = addr,
                                file = file,
                                line = line,
                            }
                        ENDLUA
                    ENDM
