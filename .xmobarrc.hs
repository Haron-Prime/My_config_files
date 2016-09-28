Config { font = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
       , additionalFonts = ["xft:Terminus:size=12:weight=bold:antialias=true:hinting=true","xft:Terminus Re33:size=4:antialias=true:hinting=true","xft:Ubuntu Mono:size=12:weight=bold:antialias=true:hinting=true"] 
       , borderColor = "#151515"
       , border = TopB
       , bgColor = "#151515"
       , fgColor = "#959595"
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
       , commands = [ Run XMonadLog
                    , Run Com "root-disk" [] "" 100
                    , Run Network "enp3s0" ["-S", "True", "-d", "1", "-t", "<fc=red>↓</fc><fn=2> </fn><rx> <fc=green>↑</fc><fn=2> </fn><tx>"] 10
                    , Run Com "Net_in" [] "" 600
                    , Run Com "Net_out" [] "" 600
                    , Run Cpu ["-t","CPU:<fn=2> </fn><total>%","-L","10","-H","75","--normal","yellow","--high","orange"] 10
                    , Run Com "cpu-temp" [] "" 30
                    , Run Com "cpufreq" [] "" 100
                    , Run Com "mb-temp" [] "" 30
                    --, Run Com "fan1" [] "" 50
                    --, Run Com "fan2" [] "" 50
                    , Run Memory ["-t", "Mem:<fn=2> </fn><usedratio>%"] 50
                    --, Run Volume "default" "Master" [] 10
                    , Run Com "vol2.sh" [] "" 10
                    , Run Kbd [("us", "<fc=#aaddff>EN</fc>"), ("ru", "<fc=orange>RU</fc>")]
                    --, Run Locks
                    , Run Com "xmdate" [] "" 600
                    , Run Com "xmtime" [] "" 10
                    , Run Com "weather" [] "" 600
                    --, Run Com "kern" [] "" 6000
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %XMonadLog% }{ <<fc=#88bbdd><</fc><fc=#aaddff><</fc><fc=#88bbdd><</fc><  %root-disk%  %enp3s0%  <fc=red>↓</fc><fn=2> </fn>%Net_in% <fc=green>↑</fc><fn=2> </fn>%Net_out%  %cpu% %cpu-temp% %cpufreq%MHz  MB:<fn=2> </fn>%mb-temp%  %memory%  Vol:<fn=2> </fn>%vol2.sh%  <fn=1>%kbd%</fn>  %xmdate% <fc=#aaddff>%xmtime%</fc>   <fn=1><fc=#aaddff>%weather%</fc></fn>"
       }
