-- Author - Haron Prime
-- License © 2017 WTFPL http://www.wtfpl.net/

Config {
         font             = "xft:Terminus Re33 Nerd Bold:size=11:antialias=true:hinting=true"
       , additionalFonts  = [
                             "xft:Terminus (TTF):size=12:weight=bold:antialias=true:hinting=true"
                            ,"xft:Terminus Re33:size=4"
                            ,"xft:Weather Icons:weight=bold:pixelsize=14"
                            ,"xft:SF Square Head:weight=bold:size=11:antialias=true:hinting=true"
                            ,"xft:Reactor Sans:weight=bold:size=8:antialias=true:hinting=true"
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
                            , Run PipeReader "/tmp/haron/TU"   "TU"
                            , Run PipeReader "/tmp/haron/TR"   "TR"
                            , Run PipeReader "/tmp/haron/CU"   "CU"
                            , Run PipeReader "/tmp/haron/TEMP" "TEMP"
                            , Run PipeReader "/tmp/haron/MEM"  "MEM"
                            , Run PipeReader "/tmp/haron/RU"   "RU"
                            , Run PipeReader "/tmp/haron/UT"   "UT"
                            , Run PipeReader "/tmp/haron/PS"   "PS"
                            , Run PipeReader "/tmp/haron/VO"   "VO"
                            , Run PipeReader "/tmp/haron/WI"   "WI"
                            , Run PipeReader "/tmp/haron/CT"   "CT"
                            , Run DynNetwork ["-S","True","-d","2","-t","<fc=#ccc></fc> <rx><fn=2> </fn><fc=#0e0></fc><fc=#e50></fc><fn=2> </fn><tx>"] 10
                            , Run Kbd        [("us", "<fn=5><fc=#9df>EN</fc></fn>"), ("ru", "<fn=5><fc=#e50>RU</fc></fn>")]
                            , Run Com        "XMTime" [] "" 10
                            ]
       , sepChar  = "%"
       , alignSep = "||"
       , template = " <action=`XMScreenfetch` button=3><action=`mygtkmenu .menurc` button=1></action></action> <action=`xdotool key 0xffeb+0xff1b` button=1><action=`mygtkmenu .workspacesrc` button=3><action=`xdotool key 0xffe3+0xffe9+0xff51` button=4><action=`xdotool key 0xffe3+0xffe9+0xff53` button=5><fn=4>%XMonadLog%</fn></action></action></action></action><fn=2>  </fn><action=`mygtkmenu .bookmarksrc` button=13></action> %MW2%||<action=XMYaourt>%UP%</action> <action=`XMVnstat-h` button=1><action=`XMVnstat-d` button=3>%dynnetwork%</action></action> <action=`xdg-open http://localhost:9091/transmission/web/` button=3><action=`XMTransgui` button=1>%TU%</action></action> <action=`XMVnstat` button=1><action=`XMVnstat-m` button=3>%TR%</action></action> <action=`XMTop-cpu` button=1><action=`XMHtop` button=3><fc=#ccc></fc><fn=2>  </fn>%CU%</action></action> <action=`XMSensors` button=13><fc=#ccc></fc><fn=2>  </fn>%TEMP%</action> <action=`XMTop-mem` button=1><action=`XMFree` button=3><fc=#ccc></fc><fn=2>  </fn>%MEM%</action></action> <action=`XMdf-h` button=1><action=`XMblkid` button=3>%RU%</action></action> <action=`XMHtop` button=1><action=`XMinxi` button=3>%UT%</action></action> <action=`XMVolUp` button=5><action=`XMVolDown` button=4><action=`XMVolMute` button=1><action=`XMPavucontrol` button=3>%VO%</action></action></action></action> <action=`XMMPC` button=45><action=`XMncmpcpp` button=3><action=`XMMPCprev` button=1></action> <action=`XMMPCtoggle` button=1><action=`XMMPCstop` button=2>%PS%</action></action> <action=`XMMPCnext` button=1></action></action></action> <action=`xkblayout-state set +1`>%kbd%</action> <action=`XMGSimplecal` button=1><action=`XMCal` button=3>%XMTime%</action></action> <action=`XMGWeather` button=13>%WI%</action><fn=2> </fn><action=`XMAccuWeather` button=1><action=`XMGismeteo` button=3><fc=#95d5f5><fn=1>%CT%</fn></fc></action></action><action=`oblogout` button=13> <fc=#ccc></fc> </action>"
       }
