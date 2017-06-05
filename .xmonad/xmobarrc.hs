-- Author - Haron Prime
-- License © 2017 WTFPL http://www.wtfpl.net/

Config {
         font             = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
       , additionalFonts  = ["xft:Terminus (TTF):size=12:weight=bold:antialias=true:hinting=true","xft:Terminus Re33:size=4"]
       , iconRoot         = "/home/haron/.xmonad/resources"
       , borderColor      = "#151515"
       , border           = NoBorder
       , bgColor          = "#151515"
       , fgColor          = "#959595"
       , alpha            = 0
       -- , position         = TopP 0 58
       , position         = Top
       , textOffset       = 15
       , iconOffset       = -1
       , lowerOnStart     = False
       , pickBroadest     = False
       , persistent       = False
       , hideOnStart      = False
       , allDesktops      = False
       , overrideRedirect = False
       , commands         = [
                              Run XMonadLog
                            , Run Com        "XMUpdate" [] "" 3000
                            , Run PipeReader "/tmp/haron/upd" "Update"
                            , Run PipeReader "/home/haron/.config/gis-weather/cur_temp" "Cur_temp"
                            , Run PipeReader "/tmp/haron/minwin" "MW"
                            , Run DynNetwork ["-S","True","-d","2","-t","<fc=#cccccc><dev>:</fc><fn=2> </fn><rx><fn=2> </fn><fc=#00dd00>↓</fc><fc=#ff6500>↑</fc><fn=2> </fn><tx>"] 10
                            , Run Com        "XMTraf" [] "" 50
                            , Run MultiCpu   ["-S","True","-t","<fc=#cccccc>CPU:</fc><fn=2> </fn><total>","-L","33","-H","66","-n","#ffff00","-h","#ff6500"] 10
                            , Run Com        "XMTCpu" [] "" 10
                            , Run Com        "XMTMB" [] "" 10
                            , Run Com        "XMTGpu" [] "" 10
                            , Run Memory     ["-S","True","-t","<fc=#cccccc>RAM:</fc><fn=2> </fn><usedratio>","-L","75","-H","90","-n","#ffff00","-h","#ff6500"] 50
                            , Run Swap       ["-S","True","-t","<fc=#cccccc>SWAP:</fc><fn=2> </fn><usedratio>","-L","75","-H","90","-n","#ffff00","-h","#ff6500"] 50
                            , Run Com        "XMRom" [] "" 100
                            , Run Com        "XMUptime" [] "" 100
                            , Run Com        "XMVol" [] "" 10
                            , Run Kbd        [("us", "<fn=1><fc=#95d5f5>EN</fc></fn>"), ("ru", "<fn=1><fc=#ff6500>RU</fc></fn>")]
                            , Run Com        "XMTime" [] "" 10
                            ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "<fn=2> </fn><action=`mygtkmenu .menurc` button=1><action=`mygtkmenu .bookmarksrc` button=3><icon=arch-mono-16x16.xpm/></action></action><action=`xdotool key 0xffeb+0xff1b`>  </action><action=`mygtkmenu .workspacesrc` button=13><action=`xdotool key 0xffe3+0xffe9+0xff51` button=4><action=`xdotool key 0xffe3+0xffe9+0xff53` button=5>%XMonadLog%</action></action></action><action=`xdotool key 0xffeb+0x60`>  </action>%MW%}{<action=XMYaourt>%Update%%XMUpdate%</action>  <action=`XMncmpcpp` button=3><action=`mpc prev` button=1><icon=backward.16x16.xpm/></action><fn=2> </fn><action=`mpc toggle` button=1><icon=pause.16x16.xpm/><icon=start.16x16.xpm/></action><action=`mpc next` button=1><icon=forward.16x16.xpm/></action></action>  <action=`XMVnstat-h` button=1><action=`XMVnstat-d` button=3>%dynnetwork%</action></action>  <action=`XMVnstat` button=1><action=`XMVnstat-m` button=3>%XMTraf%</action></action>  <action=`XMTop-cpu` button=1><action=`XMHtop` button=3>%multicpu% %XMTCpu%</action></action>  <action=`XMSensors` button=13>%XMTMB%  %XMTGpu%</action>  <action=`XMTop-mem` button=1>%memory%</action>  <action=`XMFree` button=1>%swap%</action>  <action=`XMdf-h` button=1><action=`XMblkid` button=3>%XMRom%</action></action>  %XMUptime%  <action=`pulseaudio-ctl up` button=5><action=`pulseaudio-ctl down` button=4><action=`pulseaudio-ctl mute` button=1><action=`XMPavucontrol` button=3>%XMVol%</action></action></action></action>  %kbd%  <action=`XMGSimplecal` button=1><action=`XMCal` button=3>%XMTime%</action></action>  <action=`XMAccuWeather` button=1><action=`XMGismeteo` button=3><action=`GWeather` button=45><fc=#95d5f5><fn=1>%Cur_temp%</fn></fc></action></action></action>  "
       }
