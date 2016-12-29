
-- {{{ Main
theme = {}
theme.wallpaper  = "/home/haron/Pictures/Walpapers/linux_arch_gnu_linux_1920x1080_5999.jpg"
theme.icon_theme = "AwOkenWhite"
theme.icon_dir   = "/home/haron/.icons"
-- }}}

-- {{{ Styles
theme.font        = "SonyEricsson Logo 11"
theme.myfont      = "Terminus Re33 12"

-- {{{ Colors
theme.fg_normal    = "#999999"
theme.fg_focus     = "#DDDDDD"
theme.fg_urgent    = "#CC9393"
theme.fg_myurgent  = "#FF9393"
theme.bg_normal    = "#3F3F3F"
theme.bg_focus     = "#3F3F3F"
theme.bg_urgent    = "#3F3F3F"
theme.bg_systray   = theme.bg_normal
theme.bg_myfocus   = "#3B4D5B"
-- }}}

-- {{{ Borders
theme.border_width  = 0
theme.border_normal = "#151515"
theme.border_focus  = "#99ddff"
theme.border_marked = "#CC9393"
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = "#3F3F3F"
theme.titlebar_bg_normal = "#3F3F3F"
-- }}}

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- Example:
--theme.taglist_bg_focus = "#CC9393"
-- }}}

-- {{{ Widgets
-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.fg_widget        = "#AECF96"
--theme.fg_center_widget = "#88A175"
--theme.fg_end_widget    = "#FF5656"
--theme.bg_widget        = "#494B4F"
--theme.border_widget    = "#3F3F3F"
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = "#CC9393"
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = 16
theme.menu_width  = 220
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_sel   = "/home/haron/.config/awesome/themes/haron/taglist/squarefz.png"
theme.taglist_squares_unsel = "/home/haron/.config/awesome/themes/haron/taglist/squarez.png"
theme.taglist_font          = "Terminus Re33 12"
--theme.taglist_squares_resize = "false"
-- }}}


--{{{ Applications
theme.firefox_icon	     = "/home/haron/.icons/AwOkenWhite/clear/22x22/apps/firefox.png"
theme.thunderbird_icon	 = "/home/haron/.icons/AwOkenWhite/clear/22x22/apps/thunderbird.png"
theme.vbox_icon		     = "/home/haron/.icons/AwOkenWhite/clear/22x22//mimetypes/virtualbox.png"
theme.doublecmd_icon     = "/home/haron/.icons/AwOkenWhite/clear/22x22/apps/krusader_user.png"
theme.opera_icon	     = "/home/haron/.icons/AwOkenWhite/clear/22x22/apps/opera-browser.png"
theme.kopete_icon	     = "/home/haron/.icons/AwOkenWhite/clear/22x22/apps/kopete.png"
theme.run_icon	         = "/home/haron/.icons/nouveGnomeGray/22x22/actions/system-run.png"
theme.shutdown_icon	     = "/home/haron/.icons/nouveGnomeGray/22x22/actions/system-shutdown.png"
theme.reboot_icon	     = "/home/haron/.icons/nouveGnomeGray/22x22/actions/stock_refresh.png"
theme.logout_icon	     = "/home/haron/.icons/nouveGnomeGray/22x22/actions/system-log-out.png"
theme.accesoires_icon	 = "/home/haron/.icons/nouveGnomeGray/22x22/categories/preferences-system.png"
--}}}

-- Weather
theme.weather_icon       = "/home/haron/.config/gis-weather/cur_icon.png"

-- theme.lain_icons         = os.getenv("HOME") .. "/.config/awesome/lain/icons/layout/default/"
-- theme.layout_termfair    = theme.lain_icons .. "termfairw.png"
-- theme.layout_cascade     = theme.lain_icons .. "cascadew.png"
-- theme.layout_cascadetile = theme.lain_icons .. "cascadetilew.png"
-- theme.layout_centerwork  = theme.lain_icons .. "centerworkw.png"


-- {{{ Misc
theme.awesome_icon           = "/home/haron/.config/awesome/themes/haron/awesome-icon.png"
theme.menu_submenu_icon      = "/home/haron/.config/awesome/themes/default/submenu.png"
theme.archlinux_icon	     = "/home/haron/pics//arch-mono2.png"
theme.a2_icon 	             = "/home/haron/pics/A+A.png"
-- }}}

-- {{{ Layout
theme.layout_tile       = "/home/haron/.config/awesome/themes/haron/layouts/tile.png"
theme.layout_tileleft   = "/home/haron/.config/awesome/themes/haron/layouts/tileleft.png"
theme.layout_tilebottom = "/home/haron/.config/awesome/themes/haron/layouts/tilebottom.png"
theme.layout_tiletop    = "/home/haron/.config/awesome/themes/haron/layouts/tiletop.png"
theme.layout_fairv      = "/home/haron/.config/awesome/themes/haron/layouts/fairv.png"
theme.layout_fairh      = "/home/haron/.config/awesome/themes/haron/layouts/fairh.png"
theme.layout_spiral     = "/home/haron/.config/awesome/themes/haron/layouts/spiral.png"
theme.layout_dwindle    = "/home/haron/.config/awesome/themes/haron/layouts/dwindle.png"
theme.layout_max        = "/home/haron/.config/awesome/themes/haron/layouts/max.png"
theme.layout_fullscreen = "/home/haron/.config/awesome/themes/haron/layouts/fullscreen.png"
theme.layout_magnifier  = "/home/haron/.config/awesome/themes/haron/layouts/magnifier.png"
theme.layout_floating   = "/home/haron/.config/awesome/themes/haron/layouts/floating.png"
-- }}}

-- {{{ Titlebar
theme.titlebar_close_button_focus  = "/home/haron/.config/awesome/themes/haron/titlebar/close_focus.png"
theme.titlebar_close_button_normal = "/home/haron/.config/awesome/themes/haron/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active  = "/home/haron/.config/awesome/themes/haron/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = "/home/haron/.config/awesome/themes/haron/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = "/home/haron/.config/awesome/themes/haron/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = "/home/haron/.config/awesome/themes/haron/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = "/home/haron/.config/awesome/themes/haron/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = "/home/haron/.config/awesome/themes/haron/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = "/home/haron/.config/awesome/themes/haron/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = "/home/haron/.config/awesome/themes/haron/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = "/home/haron/.config/awesome/themes/haron/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = "/home/haron/.config/awesome/themes/haron/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = "/home/haron/.config/awesome/themes/haron/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = "/home/haron/.config/awesome/themes/haron/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = "/home/haron/.config/awesome/themes/haron/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = "/home/haron/.config/awesome/themes/haron/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = "/home/haron/.config/awesome/themes/haron/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = "/home/haron/.config/awesome/themes/haron/titlebar/maximized_normal_inactive.png"
-- }}}
-- }}}

return theme
