-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
require("eminent")
-- local eminent = require("eminent")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget
-- local applicationsmenu = require("applicationsmenu")
local vicious = require("vicious")
local bashets = require("bashets")
-- local freedesktop = require("freedesktop")
local quake = require("quake")
local quakeconsole = {}
for s = 1, screen.count() do
   quakeconsole[s] = quake({ terminal = "urxvtc",
           height = 0.95,
           screen = s })
end


-- os.setlocale("ru_UA.UTF-8")
os.setlocale(os.getenv("LANG"), "time")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init("/home/haron/.config/awesome/themes/haron/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
--    awful.layout.suit.spiral,
--    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier

}
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ "W", "M", "E", "F", "S", "V", "P", "J", "T" }, s, {layouts[1], layouts[1], layouts[4], layouts[10], layouts[10], layouts[9], layouts[8], layouts[9]})
end
-- }}}

mymainmenu = awful.menu({items = {
--                                { "awesome", myawesomemenu, beautiful.awesome_icon },
                                  -- {"Accessories", applicationsmenu.applicationsmenu() ,beautiful.accesoires_icon},
                                  -- { " " },
                                  -- { "Run", "dmenu_run -i -p 'Run command:' -nb '" .. beautiful.bg_normal .. "' -nf '" .. beautiful.fg_normal .. "' -sb '" .. beautiful.bg_myfocus .. "' -sf '" .. beautiful.fg_myurgent .. "' -fn '-misc-fixed-medium-r-normal-*-15-140-75-75-c-90-iso10646-1'", beautiful.run_icon},
                                  { "Run", "dmenu_run_history -i -p 'Run:' -sb '#333' -nf '#999' -sf '#9df' -fn 'Terminus Re33:size=12'", beautiful.run_icon},
                                  -- { " " },
                                  { " Exit", awesome.quit, beautiful.logout_icon},
                                  { " Reboot", function()  awful.util.spawn_with_shell("systemctl reboot") end, beautiful.reboot_icon},
                                  { " Power off", function()  awful.util.spawn_with_shell("systemctl poweroff") end, beautiful.shutdown_icon},
                           }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.a2_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}



-- {{{ Wibox

-- {{{ My widgets

-- Language widget
local function lang_output()
    local f = io.popen("skb 1")
    local out = f:read("*a")
    f:close()
    return { out }
end
mylangbox = wibox.widget.textbox()
vicious.register(mylangbox, lang_output, "<span font=\"Terminus Re33 12\">$1</span>", 1)

-- Weather widget
local function temp_output()
    local f = io.popen("cat /home/haron/.config/gis-weather/cur_temp")
    local out = f:read("*a")
    f:close()
    return { out }
end
myweatherbox1 = wibox.widget.textbox()
vicious.register(myweatherbox1, temp_output, "<span color=\"#90d0f0\" font=\"Terminus Re33 14\">$1</span>", 10)

-- CPU widget
cpuwidget = wibox.widget.textbox()
vicious.register(cpuwidget, vicious.widgets.cpu, "☢ $1%", 1)

-- Memory widget
memwidget = wibox.widget.textbox()
vicious.register(memwidget, vicious.widgets.mem, "⛁ $1%", 1)

-- Temp CPU widget
local function temp_cpu()
    local f = io.popen("sensors | grep 'temp2' | cut -c 16-17")
    local out = f:read("*a")
    f:close()
    return { out }
end
mytempbox1 = wibox.widget.textbox()
vicious.register(mytempbox1, temp_cpu, "<span font=\"Terminus Re33 12\">+$1</span>", 1)
mytempbox1a = wibox.widget.textbox("<span font=\"Terminus Re33 11\">°C</span>")

-- Temp GPU widget
-- local function temp_gpu()
--     local f = io.popen("nvidia-settings -q gpucoretemp -t")
--     local out = f:read("*a")
--     f:close()
--     return { out }
-- end
-- mytempbox2 = wibox.widget.textbox()
-- vicious.register(mytempbox2, temp_gpu, "<span font=\"Terminus Re33 12\">+$1</span>", 5)
-- mytempbox2a = wibox.widget.textbox("<span font=\"Terminus Re33 11\">°C</span>")

-- Uptime widget
local function uptime_output()
    local f = io.popen("uptime | cut -c 11-19")
    local out = f:read("*a")
    f:close()
    return { out }
end
myuptimebox = wibox.widget.textbox()
vicious.register(myuptimebox, uptime_output, "<span font=\"Terminus Re33 12\">$1</span>", 60)

-- Netstat widget
  --Speed
netwidget = wibox.widget.textbox()
vicious.register(netwidget, vicious.widgets.net, "⬇ <span font=\"Terminus Re33 12\">${enp3s0 down_mb}M</span> <span color=\"#ffffff\"> ✦ </span>⬆ <span font=\"Terminus Re33 12\">${enp3s0 up_mb}M</span>", 1)
  --In
-- local function net_stat_in()
--     local f = io.popen("vnstat  | grep 'today' | awk '{print $2 $3}'")
--     local out = f:read("*a")
--     f:close()
--     return { out }
-- end
-- netstat_in = wibox.widget.textbox()
-- vicious.register(netstat_in, net_stat_in, "<span font=\"Terminus Re33 12\">$1</span>", 60)
  --Out
-- local function net_stat_out()
--     local f = io.popen("vnstat  | grep 'today' | awk '{print $5 $6}'")
--     local out = f:read("*a")
--     f:close()
--     return { out }
-- end
-- netstat_out = wibox.widget.textbox()
-- vicious.register(netstat_out, net_stat_out, "<span font=\"Terminus Re33 12\">$1</span>", 60)

-- MPD widget
-- mpdwidget = wibox.widget.textbox()
-- vicious.register(mpdwidget, vicious.widgets.mpd,
--     function (mpdwidget, args)
--         if args["{state}"] == "Stop" then 
--             return ""
--         else 
--             return args["{Artist}"]..' - '.. args["{Title}"]..'   '
--         end
--     end, 10)

-- Volume widget
volumewidget = wibox.widget.textbox()
vicious.register( volumewidget, vicious.widgets.volume,
  function(widget, args)
    return '<span color=\"#90d0f0\">' .. args[2] .. '</span>' .. " " .. '<span font=\"Terminus Re33 12\">' .. args[1] .. "%" .. '</span>'
  end, 1, "Master")
volumewidget:buttons(awful.util.table.join(
    awful.button({ }, 1, function () awful.util.spawn("amixer -q sset Master toggle", false) end),
    awful.button({ }, 3, function () awful.util.spawn("".. terminal.. " -e alsamixer -D equal", true) end),
    awful.button({ }, 4, function () awful.util.spawn("amixer -q sset Master 1dB+", false) end),
    awful.button({ }, 5, function () awful.util.spawn("amixer -q sset Master 1dB-", false) end)
 ))

-- Separator Widget
separator = wibox.widget.textbox()
separator: set_text ("   ")

separator2 = wibox.widget.textbox("<span color=\"#ffffff\"> ✦ </span>")

separator3 = wibox.widget.textbox()
separator3: set_text (" ")

separator4 = wibox.widget.textbox()
separator4: set_text ("/")

-- }}}

mytextclock = awful.widget.textclock("<span font=\"Terminus Re33 12\">%a, %d %b</span>", 1)
mytextclock2 = awful.widget.textclock("<span color=\"#90d0f0\" font=\"Terminus Re33 14\">%H:%M</span>", 1)

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({
                                                      theme = { width = 250 }
                                                  })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(separator3)
    left_layout:add(mylauncher)
    left_layout:add(separator3)
    left_layout:add(mytaglist[s])
    left_layout:add(separator)
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(separator2)
    -- right_layout:add(mpdwidget)
    right_layout:add(volumewidget)
    right_layout:add(separator2)
    right_layout:add(netwidget)
    -- right_layout:add(separator2)
    -- right_layout:add(netstat_in)
    -- right_layout:add(separator4)
    -- right_layout:add(netstat_out)
    right_layout:add(separator2)
    right_layout:add(cpuwidget)
    right_layout:add(separator2)
    right_layout:add(mytempbox1)
    right_layout:add(mytempbox1a)
    right_layout:add(separator2)
    right_layout:add(memwidget)
    -- right_layout:add(separator2)
    -- right_layout:add(mytempbox2)
    -- right_layout:add(mytempbox2a)
    right_layout:add(separator2)
    -- right_layout:add(myuptimebox)
    -- right_layout:add(separator2)
    right_layout:add(mylangbox)
    right_layout:add(separator2)
    right_layout:add(mytextclock)
    right_layout:add(separator2)
    right_layout:add(mytextclock2)
    right_layout:add(separator2)
    right_layout:add(myweatherbox1)
    right_layout:add(separator2)
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(separator3)
    right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",                  awful.tag.viewprev                                                                                ),
    --awful.key({                   }, "XF86Back",     awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",                 awful.tag.viewnext                                                                                ),
    --awful.key({                   }, "XF86Forward",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape",                awful.tag.history.restore                                                                         ),


    awful.key({ modkey,           }, "#44",                                                                                                                        -- Win+j
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "#45",                                                                                                                        -- Win+k
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "#25",                  function () mymainmenu:show() end), -- Win+w

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "#44",                  function () awful.client.swap.byidx(  1)                                                        end), -- Win+Shift+j
    awful.key({ modkey, "Shift"   }, "#45",                  function () awful.client.swap.byidx( -1)                                                        end), -- Win+Shift+k
    awful.key({ modkey, "Control" }, "#44",                  function () awful.screen.focus_relative( 1)                                                     end), -- Win+Ctrl+j
    awful.key({ modkey, "Control" }, "#45",                  function () awful.screen.focus_relative(-1)                                                     end), -- Win+Ctrl+k
    awful.key({ modkey,           }, "#30",                  awful.client.urgent.jumpto                                                                         ), -- Win+u
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return",               function () awful.util.spawn(terminal)                                                          end),
    awful.key({ modkey, "Control" }, "#27",                  awesome.restart                                                                                    ), -- Win+Ctrl+r
    awful.key({ modkey, "Shift"   }, "#24",                  awesome.quit                                                                                       ), -- Win+Shift+q

    awful.key({ modkey,           }, "#46",                  function () awful.tag.incmwfact( 0.05)                                                          end), -- Win+l
    awful.key({ modkey,           }, "#43",                  function () awful.tag.incmwfact(-0.05)                                                          end), -- Win+h
    awful.key({ modkey, "Shift"   }, "#43",                  function () awful.tag.incnmaster( 1)                                                            end), -- Win+Shift+h
    awful.key({ modkey, "Shift"   }, "#46",                  function () awful.tag.incnmaster(-1)                                                            end), -- Win+Shift+l
    awful.key({ modkey, "Control" }, "#43",                  function () awful.tag.incncol( 1)                                                               end), -- Win+Ctrl+h
    awful.key({ modkey, "Control" }, "#46",                  function () awful.tag.incncol(-1)                                                               end), -- Win+Ctrl+l
    awful.key({ modkey,           }, "space",                function () awful.layout.inc(layouts,  1)                                                       end),
    awful.key({ modkey, "Shift"   }, "space",                function () awful.layout.inc(layouts, -1)                                                       end),

    awful.key({ modkey, "Control" }, "#57",                  awful.client.restore                                                                               ), -- Win+Ctrl+n

    -- Prompt
    awful.key({ modkey            }, "#27",                  function () mypromptbox[mouse.screen]:run()                                                     end), -- Win+r

    awful.key({ modkey            }, "#53",                                                                                                                        -- Win+x
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),
    -- Menubar
    awful.key({ modkey            }, "#33",                  function() menubar.show()                                                                       end), -- Win+p

    -- {{{ My keys

    awful.key({                   }, "XF86Reload",           awesome.restart                                                                                    ),
    awful.key({                   }, "F12",                  function () quakeconsole[mouse.screen]:toggle()                                                 end),
    awful.key({                   }, "XF86Favorites",        function () awful.util.spawn_with_shell("transgui")                                             end),
    awful.key({                   }, "XF86Explorer",         function () awful.util.spawn_with_shell("pcmanfm")                                              end),
    awful.key({"Mod1"             }, "XF86Explorer",         function () awful.util.spawn_with_shell("gksu pcmanfm")                                         end),
    awful.key({                   }, "XF86Mail",             function () awful.util.spawn_with_shell("thunderbird")                                          end),
    awful.key({                   }, "XF86Calculator",       function () awful.util.spawn_with_shell("galculator")                                           end),
    awful.key({                   }, "XF86Tools",            function () awful.util.spawn_with_shell("urxvtc -e /usr/bin/ncmpcpp")                           end),
    awful.key({                   }, "XF86HomePage",         function () awful.util.spawn_with_shell("vivaldi-snapshot")                                     end),
    awful.key({                   }, "XF86Search",           function () awful.util.spawn_with_shell("urxvtc -name htop -e /usr/bin/htop")                   end),
    awful.key({                   }, "XF86AudioMute",        function () awful.util.spawn(           "amixer set Master toggle")                             end),
    awful.key({                   }, "XF86AudioLowerVolume", function () awful.util.spawn(           "amixer set Master 5%-")                                end),
    awful.key({                   }, "XF86AudioRaiseVolume", function () awful.util.spawn(           "amixer set Master 5%+")                                end),
    awful.key({                   }, "XF86AudioPrev",        function () awful.util.spawn(           "mpc prev")                                             end),
    awful.key({                   }, "XF86AudioNext",        function () awful.util.spawn(           "mpc next")                                             end),
    awful.key({                   }, "XF86AudioPlay",        function () awful.util.spawn(           "mpc toggle")                                           end),
    awful.key({modkey             }, "#49",                  function () awful.util.spawn(           "mpc toggle")                                           end),
    awful.key({                   }, "XF86AudioStop",        function () awful.util.spawn(           "mpc stop")                                             end),
    awful.key({                   }, "XF86Sleep",            function () awful.util.spawn(           "xautolock -locknow")                                   end),
    awful.key({                   }, "Cancel",               function () awful.util.spawn(           "/home/haron/bin/awst")                                 end),
    awful.key({                   }, "Print",                function () awful.util.spawn(           "scrot -e 'mv $f ~/Screenshots/ 2>/dev/null'")          end),
    awful.key({ "Mod1"            }, "Print",                function () awful.util.spawn(           "scrot -s -e 'mv $f ~/Screenshots/ 2>/dev/null'")       end),
    awful.key({                   }, "Menu",                 function () awful.util.spawn_with_shell("gmrun")                                                end),
    -- awful.key({ "Mod1"            }, "#31",                  function () awful.util.spawn_with_shell("/home/haron/.local/bin/iron")                          end), -- Alt+i
    -- awful.key({ "Mod1"            }, "#32",                  function () awful.util.spawn_with_shell("/home/haron/.local/bin/opera")                         end), -- Alt+o
    awful.key({ "Mod1",           }, "#33",                  function () awful.util.spawn_with_shell("env WINEPREFIX='/home/haron/.wine' wine '/home/haron/lib/Pro100-5.20-GIV/PRO100.exe'")                                                                                                                                          end), -- Alt+p
    awful.key({ "Mod1"            }, "#39",                  function () awful.util.spawn_with_shell("subl3")                                                end), -- Alt+s
    awful.key({ "Mod1"            }, "#42",                  function () awful.util.spawn_with_shell("gimp")                                                 end), -- Alt+g
    awful.key({ "Mod1"            }, "#43",                  function () awful.util.spawn_with_shell("/usr/bin/hexchat")                                     end), -- Alt+h
    awful.key({ "Mod1"            }, "#46",                  function () awful.util.spawn_with_shell("cat /home/haron/Documents/last.pass | cut -c 1-24 | xclip -selection clipboard")                                                                                                                                            end), -- Alt+l
    awful.key({ "Mod1"            }, "#25",                  function () awful.util.spawn_with_shell("gksu /usr/bin/pacmanxg")                               end), -- Alt+w
    -- awful.key({ "Mod1"            }, "#54",                  function () awful.util.spawn_with_shell("chromium")                                             end), -- Alt+c
    -- awful.key({ "Mod1", "Control" }, "#54",                  function () awful.util.spawn_with_shell("/home/haron/.local/bin/chrome --disable-setuid-sandbox")                                                                                                                                                      end), -- Alt+Ctrl+c
    awful.key({ "Mod1"            }, "#55",                  function () awful.util.spawn_with_shell("xvim")                                                 end), -- Alt+v
    -- awful.key({ "Mod1"            }, "#56",                  function () awful.util.spawn_with_shell("baobab")                                               end), -- Alt+b
    awful.key({ "Mod1"            }, "#58",                  function () awful.util.spawn_with_shell("urxvtc -name mc -e /usr/bin/mc")                       end), -- Alt+m
    awful.key({ "Mod1"            }, "F2",                   function () awful.util.spawn_with_shell("dmenu_run_history -i -p 'Run:' -sb '#333' -nf '#999' -sf '#9df' -fn 'Terminus Re33:size=12'")                                                                                                                                                      end)
    -- }}}
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "#41",                  function (c) c.fullscreen = not c.fullscreen                                                    end), -- Win+f
    awful.key({ modkey, "Shift"   }, "#54",                  function (c) c:kill()                                                                           end), -- Win+Shift+c
    awful.key({ modkey, "Control" }, "space",                awful.client.floating.toggle                                                                       ),
    awful.key({ modkey, "Control" }, "Return",               function (c) c:swap(awful.client.getmaster())                                                   end),
    awful.key({ modkey,           }, "#32",                  awful.client.movetoscreen                                                                          ), -- Win+o
    awful.key({ modkey,           }, "#28",                  function (c) c.ontop = not c.ontop                                                              end), -- Win+t
    awful.key({ modkey,           }, "#57",                                                                                                                        -- Win+n
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "#58",                                                                                                                        -- Win+m
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     border_focus = beautiful.border_focus,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
       properties = { floating = true } },
    { rule = { class = "pinentry" },
       properties = { floating = true, focus = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    { rule = { class = "galculator" },
      properties = { floating = true, focus = true } },
    { rule = { class = "hexchat" },
      properties = { floating = true, focus = true } },
    { rule = { class = "Firefox" },
       properties = {                   tag = tags[1][1], switchtotag = true, focus = true } },
    { rule = { class = "Vivaldi-snapshot" },
       properties = {                   tag = tags[1][1], switchtotag = true, focus = true } },
    { rule = { class = "Opera" },
       properties = {                   tag = tags[1][1], switchtotag = true, focus = true } },
    { rule = { class = "Iron" },
       properties = {                   tag = tags[1][1], switchtotag = true, focus = true } },
    { rule = { class = "Chromium" },
       properties = {                   tag = tags[1][1], switchtotag = true, focus = true } },
    { rule = { class = "Thunderbird" },
       properties = {                   tag = tags[1][2] } },
    { rule = { class = "Subl3"},
       properties = {                   tag = tags[1][3], switchtotag = true, focus = true } },
    { rule = { class = "Doublecmd"},
       properties = {                   tag = tags[1][4], switchtotag = true, focus = true } },
    { rule = { class = "Pcmanfm"},
       properties = {                   tag = tags[1][4], switchtotag = true, focus = true } },
    { rule = { class = "pacmanxg"},
       properties = {                   tag = tags[1][5], switchtotag = true, focus = true } },
    { rule = { class = "Baobab"},
       properties = {                   tag = tags[1][5], switchtotag = true, focus = true } },
    { rule = { class = "systemdx"},
       properties = {                   tag = tags[1][5], switchtotag = true, maximized_horizontal = true, maximized_vertical = true, focus = true } },
    { rule = { class = "VirtualBox"},
       properties = {                   tag = tags[1][5], switchtotag = true, focus = true } },
    { rule = { class = "GParted"},
       properties = {                   tag = tags[1][5], switchtotag = true, focus = true } },
    { rule = { class = "mpv"},
       properties = {                   tag = tags[1][6], switchtotag = true, focus = true } },
    { rule = { class = "Sopcast-player.py"},
       properties = {                   tag = tags[1][6], switchtotag = true, focus = true } },
    { rule = { class = "Pinta"},
       properties = {                   tag = tags[1][7], switchtotag = true, focus = true } },
    -- { rule = { class = "feh"},
    --    properties = {                   tag = tags[1][7], switchtotag = true, focus = true } },
    { rule = { class = "Gthumb"},
       properties = {                   tag = tags[1][7], switchtotag = true, focus = true } },
    { rule = { class = "Gimp"},
       properties = {                   tag = tags[1][7], switchtotag = true, focus = true } },
    { rule = { class = "Gimp-2.8"},
       properties = {                   tag = tags[1][7], switchtotag = true, focus = true } },
    { rule = { class = "Wine"},
       properties = {                   tag = tags[1][8], switchtotag = true, focus = true } },
    { rule = { class = "psi"},
       properties = { tag = tags[1][8], border_width = 0, floating = true, maximized_horizontal = false, maximized_vertical = false, focus = false } },
    { rule = { class = "Tixati"},
       properties = {                   tag = tags[1][9], switchtotag = true, focus = true } },
    { rule = { class = "Transmission-remote-gtk"},
       properties = {                   tag = tags[1][9], switchtotag = true, focus = true } },
    { rule = { class = "Transgui"},
       properties = {                   tag = tags[1][9], switchtotag = true, focus = true } },
    { rule = { class = "Deluge"},
       properties = {                   tag = tags[1][9], switchtotag = true, focus = true } },
    { rule = { class = "Conky"},
       properties = { border_width = 0, floating = true } },
    { rule = { class = "Gis-weather.py"},
       properties = { tag = tags[1][1], border_width = 0, floating = true, maximized_horizontal = false, maximized_vertical = false, focus = false } },
    { rule = { class = "Galculator" },
       properties = { floating = true, maximized_horizontal = false, maximized_vertical = false, focus = true } },
    { rule = { instance = "htop" },
       properties = { floating = true, maximized_horizontal = true, maximized_vertical = true, focus = true } },
    { rule = { instance = "mc" },
       properties = { floating = true, maximized_horizontal = true, maximized_vertical = true, focus = true } },
    { rule = { instance = "nano" },
       properties = { floating = true, maximized_horizontal = true, maximized_vertical = true, focus = true } },
    { rule = { instance = "ncmpcpp" },
       properties = { floating = true, maximized_horizontal = true, maximized_vertical = true, focus = true } }
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Run autostarting applications only once
function autostart(cmd, delay)
    delay = delay or 0
    awful.util.spawn_with_shell("pgrep -u $USER -x -f '" .. cmd .. "' || ( sleep " .. delay .. " && " .. cmd .. " )")
end

-- Autostart applications. The extra argument is optional, it means how long to
-- delay a command before starting it (in seconds).
-- autostart("compton -b", 1)
-- autostart("urxvtd -q -f -o &", 1)
autostart("python3 /home/haron/lib/gis-weather/gis-weather.py", 1)
-- }}}

-- {{{ Run .desktop files with dex
-- Because 'dex' is not an application, autostart("dex -ae Awesome") will always
-- execute every entry (which is unwanted).
local dex_output = io.popen("dex -ade Awesome")
for cmd in dex_output:lines() do
    autostart(cmd:gsub("Executing command: ", ""), 4)
end
dex_output:close()
-- }}}

