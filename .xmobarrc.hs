Config { font = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
       , additionalFonts = ["xft:Forced Square:size=12:weight=bold:antialias=true:hinting=true","xft:Terminus:size=12:weight=bold:antialias=true:hinting=true","xft:Ubuntu Mono:size=12:weight=bold:antialias=true:hinting=true"] 
       , borderColor = "#151515"
       , border = TopB
       , bgColor = "#151515"
       , fgColor = "#999"
       , alpha = 0
       --, position = Static { xpos = 0, ypos = 0, width = 1850, height = 20 }
       , position = TopP 0 62
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [Run XMonadLog
                    , Run Network "enp3s0" ["-S", "True", "-d", "1", "-t", "<rx> <tx>"] 10
                    , Run Com "Net_in" [] "" 100
                    , Run Com "Net_out" [] "" 100
                    , Run Cpu ["-L","25","-H","75","--normal","#cfdf00","--high","orange"] 10
                    , Run Com "cpu-temp" [] "" 50
                    , Run Com "mb-temp" [] "" 50
                    , Run Com "fan1" [] "" 50
                    , Run Com "fan2" [] "" 50
                    , Run Memory ["-t","Mem: <usedratio>%"] 50
                    , Run Com "vol.sh" [] "" 10
                    , Run Kbd [("us", "<fc=#aaddff>EN</fc>"), ("ru", "<fc=orange>RU</fc>")]
                    , Run Com "xmdate" [] "" 600
                    , Run Com "xmtime" [] "" 10
                    , Run Com "weather" [] "" 600
                    , Run Com "kern" [] "" 6000
                    , Run Locks 
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %XMonadLog% }{ Net: %enp3s0% In: %Net_in% Out: %Net_out% %cpu% %cpu-temp%  MB: %mb-temp%  Fan: %fan1% %fan2%  %memory%  Vol: %vol.sh%  <fn=2>%kbd%</fn>  %xmdate%  <fc=#aaddff>%xmtime%</fc>  <fn=2><fc=#afdfff>%weather%</fc></fn>"
       }

