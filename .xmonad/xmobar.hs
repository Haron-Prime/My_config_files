-- XMobar config
-- Author - Haron Prime
-- License WTFPL © 2017 http://www.wtfpl.net/

Config {
         font             = "xft:Terminus Re33 Nerd Bold:size=12:antialias=true:hinting=true"
       , additionalFonts  = [
                             "xft:Larabiefont:size=12:weight=bold:antialias=true:hinting=true"         -- fn=1
                            ,"xft:Terminus Re33 Nerd Bold:pixelsize=6"                                 -- fn=2 (for separators)
                            ,"xft:Weather Icons:weight=bold:pixelsize=14:antialias=true:hinting=true"  -- fn=3
                            ,"xft:SF Square Head:weight=bold:size=11:antialias=true:hinting=true"      -- fn=4
                            ,"xft:Terminus Re33 Nerd Bold:size=11:antialias=true:hinting=true"         -- fn=5
                            ]
       , iconRoot         = "/home/haron/.xmonad/resources"
       , borderColor      = "#151515"
       , border           = NoBorder
       , bgColor          = "#151515"
       , fgColor          = "#959595"
       , alpha            = 0
       , position         = Static { xpos = 0 , ypos = 0, width = 1920, height = 24 }
       , textOffset       = -1
       , iconOffset       = -1
       , lowerOnStart     = True
       , hideOnStart      = False
       , allDesktops      = True
       , overrideRedirect = True
       , pickBroadest     = False
       , persistent       = False
       , commands         = [
                              Run UnsafeStdinReader
                            , Run PipeReader         "/tmp/haron/UT"    "UT"      -- Uptime, load average, kernel etc
                            , Run PipeReader         "/tmp/haron/MW2"   "MW"      -- Minimized windows
                            , Run PipeReader         "/tmp/haron/DO"    "DO"      -- Current date, tz etc
                            , Run PipeReader         "/tmp/haron/TIME"  "LT"      -- Current local time
                            , Run PipeReader         "/tmp/haron/WI"    "WI"      -- Weather icon
                            , Run PipeReader         "/tmp/haron/CT"    "CT"      -- Current local temperature
                            , Run PipeReader         "/tmp/haron/UP"    "UP"      -- Availability of updates
                            , Run PipeReader         "/tmp/haron/NT"    "NT"      -- Network status
                            , Run PipeReader         "/tmp/haron/NS"    "NS"      -- Network speed
                            , Run PipeReader         "/tmp/haron/TU"    "TU"      -- Transmission-daemon status
                            , Run PipeReader         "/tmp/haron/TR"    "DT"      -- Daily traffic
                            , Run PipeReader         "/tmp/haron/CU"    "CU"      -- CPU load
                            , Run PipeReader         "/tmp/haron/TEMP"  "TA"      -- Temperature of all devices
                            , Run PipeReader         "/tmp/haron/MEM"   "MU"      -- Using RAM and SWAP
                            , Run PipeReader         "/tmp/haron/RU"    "RU"      -- Using disk partitions
                            , Run PipeReader         "/tmp/haron/VO"    "VL"      -- Volume level
                            , Run PipeReader         "/tmp/haron/PL"    "PL"      -- Playback (artist, track)
                            , Run PipeReader         "/tmp/haron/PS"    "PS"      -- MPD control
                            , Run Kbd                [
                                                       ("us", "<fc=#ccc></fc>")
                                                     , ("ru", "<fc=#f60></fc>")
                                                     ]
                            ]
       , sepChar          = "%"
       , alignSep         = "||"
       , template         = " <action=`mygtkmenu .menurc` button=1><action=`XMScreenfetch` button=3><fn=5></fn></action></action> <action=`xdotool key 0xffeb+0xff1b` button=3><action=`xdotool key 0xffe3+0xffe9+0xff51` button=4><action=`xdotool key 0xffe3+0xffe9+0xff53` button=5>%UnsafeStdinReader%</action></action></action><fn=2>  </fn><action=`mygtkmenu .placerc` button=1><action=`XMUptimeToggle` button=3><fn=5></fn></action></action>%UT%<action=`xdotool key 0xffeb+0x61`>%MW%</action>|<action=`XMDateToggle` button=1><action=`XMGSimplecal` button=3><action=`XMCal` button=45>%DO%<fn=1><fc=#ccc> %LT%</fc></fn></action></action></action>  <fc=#ccc><action=`XMGWeather` button=1>%WI%</action><fn=2> </fn><action=`XMAccuWeather` button=1><action=`XMGismeteo` button=3><fn=1>%CT%</fn></action></action></fc> |%UP% <action=`XMNetToggle` button=1><action=`xdg-open http://192.168.0.1` button=2><action=`XMVnstat & XMVnstat-h` button=3><action=`XMifconfig` button=45>%NT%</action></action></action></action>%NS% <action=`XMTrafToggle` button=1><action=`XMTransgui` button=3><action=`XMVnstat-m` button=4><action=`XMVnstat-d` button=5>%TU%</action></action></action></action>%DT%  <action=`XMCPUToggle` button=1><fc=#ccc></fc></action>%CU% <action=`XMTempToggle` button=1><action=`XMSensors` button=3><fc=#ccc></fc></action></action>%TA% <action=`XMMemToggle` button=1><fc=#ccc><fn=5></fn></fc></action>%MU%%RU% <action=`XMVolToggle` button=1><action=`XMPavucontrol` button=2><action=`pulseaudio-ctl mute` button=3><action=`pulseaudio-ctl down` button=4><action=`pulseaudio-ctl up` button=5><fc=#ccc></fc>%VL%%PL%</action></action></action></action></action>  <fn=5><action=`XMncmpcpp` button=3><action=`XMMPC` button=45><action=`XMMPCprev` button=1></action> %PS% <action=`XMMPCnext` button=1></action></action></action></fn>  <action=`xkblayout-state set +1`>%kbd%</action> <action=`XMToggleAll` button=1><action=`XMCleanAll` button=3><fc=#ccc>  </fc></action></action>"
       }
