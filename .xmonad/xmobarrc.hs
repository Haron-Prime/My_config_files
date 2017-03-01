-- Author - Haron Prime
-- License - © 2017 WTFPL - http://www.wtfpl.net/

Config {
         font             = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
       , additionalFonts  = ["xft:Terminus:size=12:weight=bold:antialias=true:hinting=true","xft:Terminus Re33:size=4"] 
       , borderColor      = "#151515"
       , border           = NoBorder
       , bgColor          = "#151515"
       , fgColor          = "#959595"
       , alpha            = 1
       , position         = TopP 1 54
       , textOffset       = 14
       , iconOffset       = 8
       , lowerOnStart     = True
       , pickBroadest     = False
       , persistent       = False
       , hideOnStart      = False
       , iconRoot         = "/home/haron/.xmonad/resources"
       , allDesktops      = True
       , overrideRedirect = True
       , commands         = [
                              Run XMonadLog
                            , Run Com        "arrows-revers" [] "" 0
                            , Run Com        "UPD-v2" [] "" 600
                            , Run DynNetwork  ["-S","True","-d","1","-t","<fc=#cccccc><dev>:</fc><fn=2> </fn><rx><fn=2> </fn><fc=#00dd00>↓</fc><fc=#ff6500>↑</fc><fn=2> </fn><tx>"] 10
                            , Run Com        "Net_in" [] "" 50
                            , Run Com        "Net_out" [] "" 50
                            , Run Cpu         ["-S","True","-t","<fc=#cccccc>CPU:</fc><fn=2> </fn><total>","-L","25","-H","75","--normal","#ffff00","--high","#ff6500"] 10
                            , Run Com        "TCPU" [] "" 10
                            , Run Com        "TMB" [] "" 10
                            , Run Com        "TGPU" [] "" 10
                            , Run Memory      ["-S","True","-t","<fc=#cccccc>RAM:</fc><fn=2> </fn><usedratio>","-L","50","-H","85","--normal","#ffff00","--high","#ff6500"] 50
                            , Run Com        "ROM" [] "" 100
                            , Run Com        "xmuptime" [] "" 100
                            , Run Com        "XVol" [] "" 10
                            , Run Kbd         [("us", "<fc=#95d5f5>EN</fc>"), ("ru", "<fc=#ff6500>RU</fc>")]
                            , Run Com        "xmdate" [] "" 10
                            , Run Com        "xmtime" [] "" 10
                            , Run Com        "myweather" [] "" 100
                            ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "<icon=arch-mono-16x16.xpm/> %XMonadLog%  %arrows-revers%  }{%UPD-v2%  %dynnetwork% %Net_in%<fn=2> </fn><fc=#00dd00>↓</fc><fc=#ff6500>↑</fc><fn=2> </fn>%Net_out%  %cpu% %TCPU%  <fc=#cccccc>MB:</fc><fn=2> </fn>%TMB%  <fc=#cccccc>GPU:</fc><fn=2> </fn>%TGPU%  %memory%  <fc=#cccccc>ROM:</fc><fn=2> </fn>%ROM%  <fc=#cccccc>Up:</fc><fn=2> </fn>%xmuptime%  <fc=#cccccc>Vol:</fc><fn=2> </fn>%XVol%  <fn=1>%kbd%</fn>  %xmdate%  <fc=#cccccc>%xmtime%</fc>  <fn=1><fc=#95d5f5>%myweather%</fc></fn>"
       }
