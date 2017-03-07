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
       , position         = TopP 0 54
       , textOffset       = 15
       , iconOffset       = -1
       , lowerOnStart     = True
       , pickBroadest     = True
       , persistent       = False
       , hideOnStart      = False
       , iconRoot         = "/home/haron/.xmonad/resources"
       , allDesktops      = True
       , overrideRedirect = True
       , commands         = [
                              Run XMonadLog
                            , Run Com        "XMNull" [] "" 0
                            , Run Com        "XMUpdate" [] "" 600
                            , Run PipeReader "/tmp/upd" "Update"
                            , Run DynNetwork ["-S","True","-d","1","-t","<fc=#cccccc><dev>:</fc><fn=2> </fn><rx><fn=2> </fn><fc=#00dd00>↓</fc><fc=#ff6500>↑</fc><fn=2> </fn><tx>"] 10
                            , Run Com        "XMTraf" [] "" 50
                            , Run Cpu        ["-S","True","-t","<fc=#cccccc>CPU:</fc><fn=2> </fn><total>","-L","25","-H","75","--normal","#ffff00","--high","#ff6500"] 10
                            , Run Com        "TCPU" [] "" 10
                            , Run Com        "TMB" [] "" 10
                            , Run Com        "TGPU" [] "" 10
                            , Run Memory     ["-S","True","-t","<fc=#cccccc>RAM:</fc><fn=2> </fn><usedratio>","-L","50","-H","85","--normal","#ffff00","--high","#ff6500"] 50
                            , Run Com        "XMRom" [] "" 100
                            , Run Com        "XMUptime" [] "" 100
                            , Run Com        "XMVol" [] "" 10
                            , Run Kbd        [("us", "<fn=1><fc=#95d5f5>EN</fc></fn>"), ("ru", "<fn=1><fc=#ff6500>RU</fc></fn>")]
                            , Run Com        "XMTime" [] "" 10
                            , Run Com        "XMWeather" [] "" 100
                            ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "<action=`i3lock -i /home/haron/wall/starrynight.png` button=1><icon=arch-mono-16x16.xpm/>%XMNull%</action> %XMonadLog%}{<action=XMYaourt>%Update%%XMUpdate%</action>  <action=`XMVnstat-h` button=1><action=`XMVnstat-d` button=3>%dynnetwork%</action></action>  <action=`XMVnstat` button=1><action=`XMVnstat-m` button=3>%XMTraf%</action></action>  <action=`XMTop-cpu` button=1><action=`XMHtop` button=3>%cpu% %TCPU%</action></action>  <action=`XMSensors` button=13>%TMB%  %TGPU%</action>  <action=`XMTop-mem` button=1><action=`XMFree` button=3>%memory%</action></action>  <action=`XMdf-h` button=1><action=`XMblkid` button=3>%XMRom%</action></action>  %XMUptime%  <action=`/usr/bin/pulseaudio-ctl up` button=5><action=`/usr/bin/pulseaudio-ctl down` button=4><action=`/usr/bin/pulseaudio-ctl mute` button=1><action=`mpc toggle` button=3>%XMVol%</action></action></action></action>  <action=`xdotool key Mode_switch` button=1>%kbd%</action>  <action=`XMCal` button=1>%XMTime%</action>  <action=`XMWeather-current` button=13>%XMWeather%</action>"
       }
