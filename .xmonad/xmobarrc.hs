-- Author - Haron Prime
-- License - © 2017 WTFPL - http://www.wtfpl.net/

Config {
         font             = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
       , additionalFonts  = ["xft:Terminus:size=12:weight=bold:antialias=true:hinting=true","xft:Terminus Re33:size=4"] 
       , borderColor      = "#151515"
       , border           = NoBorder
       , bgColor          = "#151515"
       , fgColor          = "#959595"
       , alpha            = 0
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
                            , Run Com        "XMUpdate" [] "" 3000
                            , Run PipeReader "/tmp/haron/upd" "Update"
                            , Run PipeReader "/tmp/haron/temp" "Cur_temp"
                            , Run PipeReader "/tmp/haron/minwin" "MW"
                            , Run DynNetwork ["-S","True","-d","2","-t","<fc=#cccccc><dev>:</fc><fn=2> </fn><rx><fn=2> </fn><fc=#00dd00>↓</fc><fc=#ff6500>↑</fc><fn=2> </fn><tx>"] 10
                            , Run Com        "XMTraf" [] "" 50
                            , Run Cpu        ["-S","True","-t","<fc=#cccccc>CPU:</fc><fn=2> </fn><total>","-L","33","-H","66","-n","#ffff00","-h","#ff6500"] 10
                            , Run Com        "XMTCpu" [] "" 10
                            , Run Com        "XMCpu-freq" [] "" 0
                            , Run Com        "XMTMB" [] "" 10
                            , Run Com        "XMTGpu" [] "" 10
                            , Run Memory     ["-S","True","-t","<fc=#cccccc>RAM:</fc><fn=2> </fn><usedratio>","-L","75","-H","90","-n","#ffff00","-h","#ff6500"] 50
                            , Run Com        "XMRom" [] "" 100
                            , Run Com        "XMUptime" [] "" 100
                            , Run Com        "XMVol" [] "" 10
                            , Run Kbd        [("us", "<fn=1><fc=#95d5f5>EN</fc></fn>"), ("ru", "<fn=1><fc=#ff6500>RU</fc></fn>")]
                            , Run Com        "XMTime" [] "" 10
                            ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "<action=`XMMenu` button=1><icon=arch-mono-16x16.xpm/>%XMNull%</action> %XMonadLog%  %MW%}{<action=XMYaourt>%Update%%XMUpdate%</action>  <action=`XMVnstat-h` button=1><action=`XMVnstat-d` button=3>%dynnetwork%</action></action>  <action=`XMVnstat` button=1><action=`XMVnstat-m` button=3>%XMTraf%</action></action>  <action=`XMTop-cpu` button=1><action=`XMHtop` button=3>%cpu% %XMTCpu% %XMCpu-freq%MHz</action></action>  <action=`XMSensors` button=13>%XMTMB%  %XMTGpu%</action>  <action=`XMTop-mem` button=1><action=`XMFree` button=3>%memory%</action></action>  <action=`XMdf-h` button=1><action=`XMblkid` button=3>%XMRom%</action></action>  %XMUptime%  <action=`/usr/bin/pulseaudio-ctl up` button=5><action=`/usr/bin/pulseaudio-ctl down` button=4><action=`/usr/bin/pulseaudio-ctl mute` button=1><action=`mpc toggle` button=3>%XMVol%</action></action></action></action>  %kbd%  <action=`XMGSimplecal` button=1><action=`XMCal` button=3>%XMTime%</action></action>  <action=`XMAccuWeather` button=1><action=`XMGismeteo` button=3><fn=1><fc=#95d5f5>%Cur_temp%</fc></fn></action></action>"
       }
