  ((0,               0x1008ff13),  spawn "pactl set-sink-volume 0 +5%")            --XF86AudioRaiseVolume
, ((0,               0x1008ff11),  spawn "pactl set-sink-volume 0 -5%")            --XF86AudioLowerVolume
, ((0,               0x1008ff12),  spawn "pactl set-sink-mute 0 toggle")           --XF86AudioMute
, ((altm,            0x60),  spawn "mpc toggle")                                   --Alt+grave
, ((altm,            0x61),  spawn "atom-beta")                                    --Alt+A
, ((altm,            0x65),  spawn "pulseaudio-equalizer-gtk")                     --Alt+E
, ((altm,            0x6f),  spawn "opera12")                                      --Alt+O
, ((altm,            0x70),  namedScratchpadAction mynameScratchpads "MyPlayer")   --Alt+P
, ((altm,            0x73),  spawn "subl3")                                        --Alt+S
, ((altm,            0x77),  spawn "GWeather")                                     --Alt+W
, ((altm,            0x7a),  spawn "zim")                                          --Alt+Z

, ((modm,            0x75),  sendMessage ShrinkSlave)                              --Mod4+U
, ((modm,            0x69),  sendMessage ExpandSlave)                              --Mod4+I

, ((altm .|. shftm,  0x32), spawn "XMVnstat-m")                                    --Alt+Shift+2
