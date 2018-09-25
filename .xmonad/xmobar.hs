-- XMobar config
-- Author - Haron Prime
-- License WTFPL http://www.wtfpl.net/

-- List of all scripts used and their description is in the file https://github.com/Haron-Prime/My_config_files/blob/master/.local/bin/README.md

Config {
         position         = Static { xpos = 0 , ypos = 2, width = 1920, height = 22 }
       , textOffset       = -1
       , iconOffset       = -1
       , lowerOnStart     = True
       , hideOnStart      = False
       , allDesktops      = True
       , overrideRedirect = True
       , pickBroadest     = True
       , persistent       = True
       , borderColor      = "#151515"
       , border           = NoBorder
       , bgColor          = "#151515"
       , fgColor          = "#959595"
       , alpha            = 0
       , iconRoot         = "/home/haron/.xmonad/resources"
       , font             = "xft:Terminus Re33 Nerd Bold:size=12:antialias=true:hinting=true"
       , additionalFonts  = [
                             "xft:Larabiefont:size=12:weight=bold:antialias=true:hinting=true"         -- fn=1
                            ,"xft:Terminus Re33 Nerd Bold:pixelsize=6"                                 -- fn=2 (for separators)
                            ,"xft:Weather Icons:weight=bold:pixelsize=14:antialias=true:hinting=true"  -- fn=3
                            ,"xft:SF Square Head:weight=bold:size=11:antialias=true:hinting=true"      -- fn=4 (for UnsafeStdinReader)
                            ,"xft:Terminus Re33 Nerd Bold:size=11:antialias=true:hinting=true"         -- fn=5
                            ]
       , commands         = [
                              Run UnsafeStdinReader
                            , Run PipeReader         "/tmp/haron/UT"    "UT"                           -- Uptime, load average, kernel etc
                            , Run PipeReader         "/tmp/haron/MW"    "MW"                           -- Minimized windows
                            , Run PipeReader         "/tmp/haron/DO"    "DO"                           -- Current local date, tz etc
                            , Run PipeReader         "/tmp/haron/LT"    "LT"                           -- Current local time
                            , Run PipeReader         "/tmp/haron/WI"    "WI"                           -- Current weather icon
                            , Run PipeReader         "/tmp/haron/CT"    "CT"                           -- Current local temperature
                            , Run PipeReader         "/tmp/haron/UP"    "UP"                           -- Availability of updates
                            , Run PipeReader         "/tmp/haron/NT"    "NT"                           -- Network status
                            , Run PipeReader         "/tmp/haron/NS"    "NS"                           -- Network speed
                            , Run PipeReader         "/tmp/haron/TS"    "TS"                           -- Transmission-daemon status
                            , Run PipeReader         "/tmp/haron/DT"    "DT"                           -- Daily traffic
                            , Run PipeReader         "/tmp/haron/CU"    "CU"                           -- CPU load and freq
                            , Run PipeReader         "/tmp/haron/TA"    "TA"                           -- Temperature of all devices
                            , Run PipeReader         "/tmp/haron/MU"    "MU"                           -- Using RAM and SWAP
                            , Run PipeReader         "/tmp/haron/RU"    "RU"                           -- Using disk partitions
                            , Run PipeReader         "/tmp/haron/VS"    "VS"                           -- Volume status
                            , Run PipeReader         "/tmp/haron/VL"    "VL"                           -- Volume level
                            , Run PipeReader         "/tmp/haron/PL"    "PL"                           -- Playback (artist/track)
                            , Run PipeReader         "/tmp/haron/PS"    "PS"                           -- MPD control buttons
                            , Run PipeReader         "/tmp/haron/SC"    "SC"                           -- SoundCloud player status
                            , Run Kbd                [
                                                       ("us", "<fc=#0e0></fc>")
                                                     , ("ru", "<fc=#f60></fc>")
                                                     ]
                            ]
       , sepChar          = "%"
       , alignSep         = "||"
       , template         = " <action=`mygtkmenu .menurc` button=1><fn=5></fn></action> <action=`xdotool key 0xffeb+0xff1b` button=3><action=`xdotool key 0xffe3+0xffe9+0xff51` button=4><action=`xdotool key 0xffe3+0xffe9+0xff53` button=5>%UnsafeStdinReader%</action></action></action> <action=`mygtkmenu .placerc` button=1><action=`XMUptimeToggle` button=3><fn=5></fn></action></action>%UT%<action=`xdotool key 0xffeb+0xffe1+0x7a`>%MW%</action>|<action=`XMDateToggle` button=1><action=`XMGSimplecal` button=3><action=`XMCal` button=45>%DO%<fn=1><fc=#ccc> %LT%</fc></fn></action></action></action>  <fc=#ccc><action=`XMGWeather` button=1>%WI%</action><fn=2> </fn><action=`XMAccuWeather` button=1><action=`XMOWM` button=3><fn=1>%CT%</fn></action></action></fc>|%UP%  <action=`XMNetToggle` button=1><action=`xdg-open http://192.168.0.1` button=2><action=`XMVnstat+h` button=3><action=`XMifconfig` button=45>%NT%%NS%</action></action></action></action>  <action=`XMTrafToggle` button=1><action=`XMTransgui` button=3><action=`XMVnstat-m` button=4><action=`XMVnstat-d` button=5>%TS%</action></action></action></action>%DT%  <action=`XMCPUToggle` button=1><fc=#ccc></fc></action>%CU%  <action=`XMTempToggle` button=1><action=`XMSensors` button=3><fc=#ccc></fc></action></action>%TA%  <action=`XMMemToggle` button=1><fc=#ccc></fc></action>%MU%%RU%  <action=`XMVolToggle` button=1><action=`XMPavucontrol` button=2><action=`XMMute` button=3><action=`pulseaudio-ctl down` button=4><action=`pulseaudio-ctl up` button=5>%VS%%VL%%PL%</action></action></action></action></action>  %PS%  <action=`XMSoundCloud`>%SC%</action>  <action=`xkblayout-state set +1`>%kbd%</action> <action=`XMToggleAll` button=1><action=`XMCleanAll` button=3><fc=#ccc>  </fc></action></action>"
       }
