-- Author - Haron Prime
-- License © 2017 WTFPL http://www.wtfpl.net/

Config {
         font             = "xft:Terminus Re33 Nerd Bold:size=12:antialias=true:hinting=true"
       , additionalFonts  = [
                             "xft:Larabiefont:size=12:weight=bold:antialias=true:hinting=true"
                            ,"xft:Terminus Re33:pixelsize=8"
                            ,"xft:Weather Icons:weight=bold:pixelsize=14"
                            ,"xft:SF Square Head:weight=bold:size=11:antialias=true:hinting=true"
                            ,"xft:Terminus Re33 Nerd Bold:size=11:antialias=true:hinting=true"
                            ]
       , iconRoot         = "/home/haron/.xmonad/resources"
       , borderColor      = "#151515"
       , border           = NoBorder
       , bgColor          = "#151515"
       , fgColor          = "#959595"
       , alpha            = 0
       , position         = Top
       , textOffset       = 16
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
                            , Run PipeReader "/tmp/haron/NT"   "NT"
                            , Run PipeReader "/tmp/haron/NS"   "NS"
                            , Run PipeReader "/tmp/haron/TU"   "TU"
                            , Run PipeReader "/tmp/haron/TR"   "TR"
                            , Run PipeReader "/tmp/haron/CU"   "CU"
                            , Run PipeReader "/tmp/haron/TEMP" "TEMP"
                            , Run PipeReader "/tmp/haron/MEM"  "MEM"
                            , Run PipeReader "/tmp/haron/RU"   "RU"
                            , Run PipeReader "/tmp/haron/UT"   "UT"
                            , Run PipeReader "/tmp/haron/VO"   "VO"
                            , Run PipeReader "/tmp/haron/PS"   "PS"
                            , Run PipeReader "/tmp/haron/DO"   "DO"
                            , Run PipeReader "/tmp/haron/WI"   "WI"
                            , Run PipeReader "/tmp/haron/CT"   "CT"
                            , Run Kbd        [("us", "<fc=#9df></fc>"), ("ru", "<fc=#e50></fc>")]
                            , Run Com        "date" ["+%H:%M:%S"] "Time" 10
                            ]
       , sepChar  = "%"
       , alignSep = "||"
       , template = " <action=`XMScreenfetch` button=3><action=`mygtkmenu .menurc` button=1><fn=5></fn></action></action> <action=`xdotool key 0xffeb+0xff1b` button=1><action=`mygtkmenu .workspacesrc` button=3><action=`xdotool key 0xffe3+0xffe9+0xff51` button=4><action=`xdotool key 0xffe3+0xffe9+0xff53` button=5><fn=4>%XMonadLog%</fn></action></action></action></action><fn=2>  </fn><action=`mygtkmenu .placerc` button=1><fn=5></fn></action> %MW2%||<action=XMYaourt>%UP%</action> <action=`XMNetState` button=1><action=`XMifconfig` button=3><action=`XMVnstat-h & XMVnstat` button=45>%NT%</action></action></action>%NS% <action=`XMTrafState` button=1><action=`XMTransgui` button=3><action=`XMVnstat-m` button=4><action=`XMVnstat-d` button=5>%TU%</action></action></action></action>%TR% <action=`XMCPUState` button=1><fc=#ccc></fc></action>%CU% <action=`XMTempState` button=1><action=`XMSensors` button=3><fc=#ccc></fc></action></action>%TEMP% <action=`XMMemState` button=1><fc=#ccc><fn=5></fn></fc></action>%MEM%%RU% <action=`XMUptimeState` button=1><action=`XMinxi` button=3><fc=#9df></fc></action></action>%UT% <action=`XMVolState` button=1><action=`XMPavucontrol` button=2><action=`pulseaudio-ctl mute` button=3><action=`pulseaudio-ctl up` button=5><action=`pulseaudio-ctl down` button=4><fc=#ccc></fc>%VO%</action></action></action></action></action> <fn=5><action=`XMMPC` button=45><action=`XMncmpcpp` button=3><action=`XMMPCprev` button=1></action> <action=`XMMPCtoggle` button=1><action=`XMMPCstop` button=2>%PS%</action></action> <action=`XMMPCnext` button=1></action></action></action></fn> <action=`xkblayout-state set +1`>%kbd%</action><action=`XMDateState` button=1><action=`XMGSimplecal` button=4><action=`XMCal` button=5>%DO% <fn=1><fc=#ccc>%Time%</fc></fn></action></action></action> <action=`XMGWeather` button=1>%WI%</action><fn=2> </fn><action=`XMAccuWeather` button=1><action=`XMGismeteo` button=3><fc=#ccc><fn=1>%CT%C</fn></fc></action></action><action=`XMStateAll` button=1><action=`XMKill` button=3> <fc=#ccc></fc> </action></action>"
       }
