-- Author - Haron Prime
-- License © 2017 WTFPL http://www.wtfpl.net/

Config {
         font             = "xft:Terminus Re33 Nerd Bold:size=12:antialias=true:hinting=true"
       , additionalFonts  = [
                             "xft:Larabiefont:size=12:weight=bold:antialias=true:hinting=true"
                            ,"xft:Terminus Re33:pixelsize=6"
                            ,"xft:Weather Icons:weight=bold:pixelsize=14:antialias=true:hinting=true"
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
       , textOffset       = -1
       , iconOffset       = -1
       , lowerOnStart     = True
       , pickBroadest     = False
       , persistent       = False
       , hideOnStart      = False
       , allDesktops      = True
       , overrideRedirect = True
       , commands         = [
                              Run UnsafeStdinReader
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
                            , Run PipeReader "/tmp/haron/PL"   "PL"
                            , Run PipeReader "/tmp/haron/PS"   "PS"
                            , Run PipeReader "/tmp/haron/DO"   "DO"
                            , Run PipeReader "/tmp/haron/WI"   "WI"
                            , Run PipeReader "/tmp/haron/CT"   "CT"
                            , Run PipeReader "/tmp/haron/TIME" "TIME"
                            , Run Kbd        [("us", "<fn=5><fc=#9df></fc></fn>"), ("ru", "<fn=5><fc=#e50></fc></fn>")]
                            ]
       , sepChar          = "%"
       , alignSep         = "||"
       , template         = " <action=`mygtkmenu .menurc` button=1><action=`XMScreenfetch` button=3><fn=5></fn></action></action> <action=`xdotool key 0xffeb+0xff1b` button=3><action=`xdotool key 0xffe3+0xffe9+0xff51` button=4><action=`xdotool key 0xffe3+0xffe9+0xff53` button=5>%UnsafeStdinReader%</action></action></action><fn=2>  </fn><action=`mygtkmenu .placerc` button=1><action=`XMUptimeState` button=3><fn=5></fn></action></action> %UT%<action=`xdotool key 0xffeb+0x61`>%MW2%</action>| <action=`XMDateState` button=1><action=`XMGSimplecal` button=3><action=`XMCal` button=45>%DO%<fn=1><fc=#ccc> %TIME%</fc></fn></action></action></action> |%UP% <action=`XMNetState` button=1><action=`xdg-open http://192.168.0.1` button=2><action=`XMVnstat & XMVnstat-h` button=3><action=`XMifconfig` button=45>%NT%</action></action></action></action>%NS% <action=`XMTrafState` button=1><action=`XMTransgui` button=3><action=`XMVnstat-m` button=4><action=`XMVnstat-d` button=5>%TU%</action></action></action></action>%TR%  <action=`XMCPUState` button=1><fc=#ccc></fc></action>%CU% <action=`XMTempState` button=1><action=`XMSensors` button=3><fc=#ccc></fc></action></action>%TEMP% <action=`XMMemState` button=1><fc=#ccc><fn=5></fn></fc></action>%MEM%%RU% <action=`XMVolState` button=1><action=`XMPavucontrol` button=2><action=`pulseaudio-ctl mute` button=3><action=`pulseaudio-ctl down` button=4><action=`pulseaudio-ctl up` button=5><fc=#ccc></fc>%VO%%PL%</action></action></action></action></action>  <fn=5><action=`XMncmpcpp` button=3><action=`XMMPC` button=45><action=`XMMPCprev` button=1></action> %PS% <action=`XMMPCnext` button=1></action></action></action></fn>  <action=`xkblayout-state set +1`>%kbd%</action>  <fc=#ccc><action=`XMGWeather` button=1>%WI%</action><fn=2> </fn><action=`XMAccuWeather` button=1><action=`XMGismeteo` button=3><fn=1>%CT%C</fn></action></action></fc> <action=`XMStateAll` button=1><action=`XMStateKill` button=3><fc=#ccc>  </fc></action></action>"
       }
