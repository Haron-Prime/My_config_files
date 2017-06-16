-- Author - Haron Prime
-- License © 2017 WTFPL http://www.wtfpl.net/

Config {
         font             = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
       , additionalFonts  = ["xft:Terminus (TTF):size=12:weight=bold:antialias=true:hinting=true","xft:Terminus Re33:size=4","xft:FontAwesome:pixelsize=14","xft:Weather Icons:pixelsize=14"]
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
                            , Run PipeReader "/tmp/haron/upd" "Update"
                            , Run PipeReader "/tmp/haron/cur_temp" "CT"
                            , Run PipeReader "/tmp/haron/minwin" "MW"
                            , Run Com        "XMUpdate" [] "" 3000
                            , Run Com        "XMTrans" [] "" 100
                            , Run DynNetwork ["-S","True","-d","2","-t","<fc=#cccccc><dev>:</fc><fn=2> </fn><rx><fn=2> </fn><fc=#00ff00>↓</fc><fc=#ff6500>↑</fc><fn=2> </fn><tx>"] 10
                            , Run Com        "XMTraf" [] "" 50
                            , Run MultiCpu   ["-S","True","-t","<fc=#cccccc>CPU:</fc><fn=2> </fn><total>","-L","33","-H","66","-n","#ffff00","-h","#ff6500"] 10
                            , Run Com        "XMTCpu" [] "" 10
                            , Run Com        "XMTMB" [] "" 10
                            , Run Com        "XMTGpu" [] "" 10
                            , Run Memory     ["-S","True","-t","<fc=#cccccc>RAM:</fc><fn=2> </fn><usedratio>","-L","75","-H","90","-n","#ffff00","-h","#ff6500"] 50
                            , Run Swap       ["-S","True","-t","<usedratio>","-L","50","-H","75","-n","#ffff00","-h","#ff6500"] 50
                            , Run Com        "XMRom" [] "" 100
                            , Run Com        "XMUptime" [] "" 100
                            , Run Com        "XMVol" [] "" 10
                            , Run Com        "XMMPD" [] "" 10
                            , Run Kbd        [("us", "<fn=1><fc=#95d5f5>EN</fc></fn>"), ("ru", "<fn=1><fc=#ff6500>RU</fc></fn>")]
                            , Run Com        "XMTime" [] "" 10
                            , Run Com        "XMWeatherIcon" [] "" 100
                            , Run Com        "XMDropbox" [] "" 100
                            ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "<fn=2> </fn><action=`XMScreenfetch` button=13><icon=arch-mono-16x16.xpm/></action> <action=`mygtkmenu .menurc` button=13><fn=3></fn></action> <action=`xdotool key 0xffeb+0xff1b` button=1><action=`mygtkmenu .workspacesrc` button=3><action=`xdotool key 0xffe3+0xffe9+0xff51` button=4><action=`xdotool key 0xffe3+0xffe9+0xff53` button=5>%XMonadLog%</action></action></action></action> <action=`xdotool key 0xffeb+0x60` button=13><fn=3></fn></action>  %MW%}{<action=XMYaourt>%Update%%XMUpdate%</action>  <action=`XMVnstat-h` button=1><action=`XMVnstat-d` button=3>%dynnetwork%</action></action> <action=`xdotool key 0xffeb+0x39` button=1><action=`xdg-open http://localhost:9091/transmission/web/` button=2><action=`XMTransgui` button=3>%XMTrans%</action></action></action> <action=`XMVnstat` button=1><action=`XMVnstat-m` button=3>%XMTraf%</action></action>  <action=`XMTop-cpu` button=1><action=`XMHtop` button=3>%multicpu%</action></action> <action=`XMSensors` button=13>%XMTCpu%  %XMTMB%  %XMTGpu%</action>  <action=`XMTop-mem` button=13>%memory%</action> <action=`XMFree` button=13>%swap%</action>  <action=`XMdf-h` button=1><action=`XMblkid` button=3>%XMRom%</action></action>  <action=`XMHtop` button=1><action=`XMinxi` button=3>%XMUptime%</action></action>  <action=`pulseaudio-ctl up` button=5><action=`pulseaudio-ctl down` button=4><action=`pulseaudio-ctl mute` button=1><action=`XMPavucontrol` button=3>%XMVol%</action></action></action></action>  <action=`XMMPC` button=45><action=`XMncmpcpp` button=3><action=`mpc prev` button=1><fn=3></fn></action> <action=`mpc toggle` button=1><action=`mpc stop` button=2>%XMMPD%</action></action> <action=`mpc next` button=1><fn=3></fn></action></action></action>  <action=`xkblayout-state set +1`>%kbd%</action>  <action=`XMGSimplecal` button=1><action=`XMCal` button=3>%XMTime%</action></action>  <action=`GWeather` button=13><fc=#cccccc>%XMWeatherIcon%</fc></action><fn=2> </fn><action=`XMAccuWeather` button=1><action=`XMGismeteo` button=3><fc=#95d5f5><fn=1>%CT%</fn></fc></action></action><action=`killall dropbox` button=3>%XMDropbox%</action><action=`mygtkmenu .bookmarksrc` button=13><fc=#cccccc> <fn=3></fn> </fc></action>"
       }
