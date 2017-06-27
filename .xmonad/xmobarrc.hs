-- Author - Haron Prime
-- License © 2017 WTFPL http://www.wtfpl.net/

Config {
         font             = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
       , additionalFonts  = [
                             "xft:Terminus (TTF):size=12:weight=bold:antialias=true:hinting=true"
                            ,"xft:Terminus Re33:size=4"
                            ,"xft:FontAwesome:pixelsize=14"
                            ,"xft:Weather Icons:weight=bold:pixelsize=14"
                            ,"xft:Terminus Re33:size=8"
                            ]
       , iconRoot         = "/home/haron/.xmonad/resources"
       , borderColor      = "#151515"
       , border           = NoBorder
       , bgColor          = "#151515"
       , fgColor          = "#959595"
       , alpha            = 0
       , position         = Top
       , textOffset       = -1
       , iconOffset       = -1
       , lowerOnStart     = True
       , pickBroadest     = False
       , persistent       = False
       , hideOnStart      = False
       , allDesktops      = False
       , overrideRedirect = True
       , commands         = [
                              Run XMonadLog
                            , Run PipeReader "/tmp/haron/UP"   "UP"
                            , Run PipeReader "/tmp/haron/MW2"  "MW2"
                            , Run PipeReader "/tmp/haron/CU"   "CU"
                            , Run PipeReader "/tmp/haron/TCPU" "TCPU"
                            , Run PipeReader "/tmp/haron/TMB"  "TMB"
                            , Run PipeReader "/tmp/haron/TGPU" "TGPU"
                            , Run PipeReader "/tmp/haron/MU"   "MU"
                            , Run PipeReader "/tmp/haron/SU"   "SU"
                            , Run PipeReader "/tmp/haron/RU"   "RU"
                            , Run PipeReader "/tmp/haron/VO"   "VO"
                            , Run PipeReader "/tmp/haron/WI"   "WI"
                            , Run PipeReader "/tmp/haron/CT"   "CT"
                            , Run Com        "XMTrans" [] "" 100
                            , Run DynNetwork ["-S","True","-d","2","-t","<fc=#ccc><fn=3></fn></fc> <rx><fn=2> </fn><fc=#0e0>↓</fc><fc=#e50>↑</fc><fn=2> </fn><tx>"] 10
                            , Run Com        "XMTraf" [] "" 50
                            , Run Com        "XMUptime" [] "" 600
                            , Run Com        "XMMPD" [] "" 10
                            , Run Kbd        [("us", "<fn=1><fc=#9df>EN</fc></fn>"), ("ru", "<fn=1><fc=#e50>RU</fc></fn>")]
                            , Run Com        "XMTime" [] "" 10
                            ]
       , sepChar  = "%"
       , alignSep = "||"
       , template = "<fn=2> </fn><action=`XMScreenfetch` button=13><icon=arch-mono-16x16.xpm/></action> <action=`mygtkmenu .menurc` button=13><fn=3></fn></action> <action=`xdotool key 0xffeb+0xff1b` button=1><action=`mygtkmenu .workspacesrc` button=3><action=`xdotool key 0xffe3+0xffe9+0xff51` button=4><action=`xdotool key 0xffe3+0xffe9+0xff53` button=5>%XMonadLog%</action></action></action></action> <action=`mygtkmenu .bookmarksrc` button=13><fn=3></fn></action>  %MW2%||<action=XMYaourt>%UP%</action> <action=`XMVnstat-h` button=1><action=`XMVnstat-d` button=3>%dynnetwork%</action></action> <action=`xdotool key 0xffeb+0x39` button=1><action=`xdg-open http://localhost:9091/transmission/web/` button=2><action=`XMTransgui` button=3>%XMTrans%</action></action></action> <action=`XMVnstat` button=1><action=`XMVnstat-m` button=3>%XMTraf%</action></action> <action=`XMTop-cpu` button=1><action=`XMHtop` button=3><fc=#ccc><fn=3></fn></fc> %CU%</action></action> <action=`XMSensors` button=13>%TCPU% %TMB% %TGPU%</action> <action=`XMTop-mem` button=13><fc=#ccc><fn=3></fn></fc> %MU%</action> <action=`XMFree` button=13>%SU%</action> <action=`XMdf-h` button=1><action=`XMblkid` button=3>%RU%</action></action> <action=`XMHtop` button=1><action=`XMinxi` button=3>%XMUptime%</action></action> <action=`XMVolUp` button=5><action=`XMVolDown` button=4><action=`XMVolMute` button=1><action=`XMPavucontrol` button=3>%VO%</action></action></action></action> <action=`XMMPC` button=45><action=`XMncmpcpp` button=3><action=`mpc prev` button=1><fn=3></fn></action> <action=`mpc toggle` button=1><action=`mpc stop` button=2>%XMMPD%</action></action> <action=`mpc next` button=1><fn=3></fn></action></action></action> <action=`xkblayout-state set +1`>%kbd%</action> <action=`XMGSimplecal` button=1><action=`XMCal` button=3>%XMTime%</action></action> <action=`GWeather` button=13>%WI%</action><fn=2> </fn><action=`XMAccuWeather` button=1><action=`XMGismeteo` button=3><fc=#95d5f5><fn=1>%CT%</fn></fc></action></action><action=`XMXClock` button=13> <fc=#ccc><fn=3></fn></fc> </action>"
       }
