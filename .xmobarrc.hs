Config { font = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
       , additionalFonts = ["xft:Terminus:size=12:weight=bold:antialias=true:hinting=true","xft:Terminus Re33:size=4:antialias=true:hinting=true"] 
       , borderColor = "#151515"
       , border = NoBorder
       , bgColor = "#151515"
       , fgColor = "#959595"
       , alpha = 1
       --, position = Static { xpos = 0, ypos = 0, width = 1850, height = 20 }
       , position = TopP 1 54
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "/home/haron/.xmonad/resources"
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run XMonadLog
                    , Run Network "enp3s0" ["-S", "True", "-d", "1", "-t", "<fc=#cccccc>Net:</fc><fn=2> </fn><rx><fn=2> </fn><fc=green>↓</fc><fc=#ff6500>↑</fc><fn=2> </fn><tx>"] 10
                    , Run Com "Net_in" [] "" 50
                    , Run Com "Net_out" [] "" 50
                    , Run Com "Net_all" [] "" 50
                    , Run Cpu ["-t","<fc=#cccccc>CPU:</fc><fn=2> </fn><total>%","-L","25","-H","75","--normal","yellow","--high","#ff6500"] 10
                    , Run Com "TCPU" [] "" 10
                    , Run Com "TMB" [] "" 10
                    , Run Com "TGPU" [] "" 10
                    , Run Memory ["-t", "<fc=#cccccc>RAM:</fc><fn=2> </fn><usedratio>%","-L","50","-H","85","--normal","yellow","--high","#ff6500"] 50
                    , Run Com "ROM" [] "" 100
                    , Run Com "xmuptime" [] "" 600
                    , Run Com "XVol" [] "" 10
                    , Run Kbd [("us", "<fc=#95d5f5>EN</fc>"), ("ru", "<fc=#ff6500>RU</fc>")]
                    , Run Com "xmdate" [] "" 10
                    , Run Com "xmtime" [] "" 10
                    , Run Com "myweather" [] "" 600
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=arch-16x16.xpm/> %XMonadLog% }{ <<fc=#85b5d5><</fc><fc=#95d5f5><</fc><fc=#85b5d5><</fc>< %enp3s0%  <fc=#cccccc>Traf:</fc><fn=2> </fn>%Net_in%<fn=2> </fn><fc=green>↓</fc><fc=#ff6500>↑</fc><fn=2> </fn>%Net_out%  <fc=#cccccc>Σ:</fc><fn=2> </fn>%Net_all%  %cpu% %TCPU%  <fc=#cccccc>MB:</fc><fn=2> </fn>%TMB%  <fc=#cccccc>GPU:</fc><fn=2> </fn>%TGPU%  %memory%  <fc=#cccccc>ROM:</fc><fn=2> </fn>%ROM%  <fc=#cccccc>Up:</fc><fn=2> </fn>%xmuptime%  <fc=#cccccc>Vol:</fc><fn=2> </fn>%XVol%  <fn=1>%kbd%</fn>  %xmdate%  <fc=#cccccc>%xmtime%</fc>  <fn=1><fc=#95d5f5>%myweather%</fc></fn>"
       }
