, ((mod1Mask,                        0x60),  spawn "mpc toggle")                                     --Alt+grave
, ((mod1Mask,                        0x61),  spawn "atom-beta")                                      --Alt+A
, ((mod1Mask,                        0x65),  spawn "pulseaudio-equalizer-gtk")                       --Alt+E
, ((mod1Mask,                        0x6f),  spawn "opera12")                                        --Alt+O
, ((mod1Mask,                        0x70),  namedScratchpadAction mynameScratchpads "MyPlayer")     --Alt+P
, ((mod1Mask,                        0x73),  spawn "subl3")                                          --Alt+S
, ((mod1Mask,                        0x77),  spawn "GWeather")                                       --Alt+W
, ((mod1Mask,                        0x7a),  spawn "zim")                                            --Alt+Z

, ((modm,                            0x75),  sendMessage ShrinkSlave)                                --Mod4+U
, ((modm,                            0x69),  sendMessage ExpandSlave)                                --Mod4+I

, ((mod1Mask .|. shiftMask,          0x32), spawn "XMVnstat-m")                                      --Alt+Shift+2
