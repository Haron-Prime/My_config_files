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
                            , Run Com        "XMUpdate" [] "" 600
                            , Run DynNetwork  ["-S","True","-d","1","-t","<fc=#cccccc><dev>:</fc><fn=2> </fn><rx><fn=2> </fn><fc=#00dd00>↓</fc><fc=#ff6500>↑</fc><fn=2> </fn><tx>"] 10
                            , Run Com        "XMTraf" [] "" 50
                            , Run Cpu         ["-S","True","-t","<fc=#cccccc>CPU:</fc><fn=2> </fn><total>","-L","25","-H","75","--normal","#ffff00","--high","#ff6500"] 10
                            , Run Com        "TCPU" [] "" 10
                            , Run Com        "TMB" [] "" 10
                            , Run Com        "TGPU" [] "" 10
                            , Run Memory      ["-S","True","-t","<fc=#cccccc>RAM:</fc><fn=2> </fn><usedratio>","-L","50","-H","85","--normal","#ffff00","--high","#ff6500"] 50
                            , Run Com        "ROM" [] "" 100
                            , Run Com        "XMUptime" [] "" 100
                            , Run Com        "XMVol" [] "" 10
                            , Run Kbd         [("us", "<fn=1><fc=#95d5f5>EN</fc></fn>"), ("ru", "<fn=1><fc=#ff6500>RU</fc></fn>")]
                            , Run Com        "XMTime" [] "" 10
                            , Run Com        "XMWeather" [] "" 100
                            ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "<icon=arch-mono-16x16.xpm/> %XMonadLog%}{%XMUpdate%  %dynnetwork%  <action=vnstat | xargs -0 notify-send -t 0>%XMTraf%</action>  <action=urxvt -name htop -e htop>%cpu% %TCPU%</action>  %TMB%  %TGPU%  <action=free | xargs -0 notify-send -t 0>%memory%</action>  <action=df -h | xargs -0 notify-send -t 0>%ROM%</action>  %XMUptime%  %XMVol%  %kbd%  <action=`xclock`>%XMTime%</action>  <action=cal | xargs -0 notify-send -t 0>%XMWeather%</action>"
       }
