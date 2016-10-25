Config { font = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
       , additionalFonts = ["xft:Terminus:size=12:weight=bold:antialias=true:hinting=true","xft:Terminus Re33:size=4:antialias=true:hinting=true"] 
       -- , borderColor = "#151515"
       , border = NoBorder
       , bgColor = "#151515"
       , fgColor = "#959595"
       , alpha = 1
       --, position = Static { xpos = 0, ypos = 0, width = 1850, height = 20 }
       , position = TopP 0 54
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
                    , Run Network "enp3s0" ["-S", "True", "-d", "1", "-t", "Net:<fn=2> </fn><rx><fn=2> </fn><fc=green>↓</fc><fc=red>↑</fc><fn=2> </fn><tx>"] 10
                    , Run Com "Net_in" [] "" 600
                    , Run Com "Net_out" [] "" 600
                    , Run Cpu ["-t","CPU:<fn=2> </fn><total>%","-L","25","-H","75","--normal","yellow","--high","orange"] 10
                    , Run Com "TCpu" [] "" 10
                    -- , Run Com "cpu-freq" [] "" 100
                    , Run Com "TMB" [] "" 10
                    , Run Com "TNV" [] "" 10
                    , Run Memory ["-t", "RAM:<fn=2> </fn><usedratio>%","-L","50","-H","85","--normal","yellow","--high","orange"] 50
                    , Run Com "ROM" [] "" 100
                    , Run Com "xmuptime" [] "" 600
                    , Run Com "XVol" [] "" 10
                    , Run Kbd [("us", "<fc=#aaddff>EN</fc>"), ("ru", "<fc=orange>RU</fc>")]
                    , Run Com "xmdate" [] "" 600
                    , Run Com "xmtime" [] "" 10
                    , Run Com "weather" [] "" 600
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %XMonadLog% }{ <<fc=#88bbdd><</fc><fc=#aaddff><</fc><fc=#88bbdd><</fc><  %enp3s0%  %Net_in%<fn=2> </fn><fc=green>↓</fc><fc=red>↑</fc><fn=2> </fn>%Net_out%  %cpu% %TCpu%  MB:<fn=2> </fn>%TMB%  GPU:<fn=2> </fn>%TNV%  %memory%  ROM:<fn=2> </fn>%ROM%  Up:<fn=2> </fn>%xmuptime%  Vol:<fn=2> </fn>%XVol%  <fn=1>%kbd%</fn>  %xmdate%  <fc=#aaddff>%xmtime%</fc>  <fn=1><fc=#aaddff>%weather%</fc></fn>"
       }
