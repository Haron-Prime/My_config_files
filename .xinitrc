#!/bin/sh
export LANG=ru_UA.UTF-8
export MM_CHARSET=UTF-8
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

if [ -d /etc/X11/xinit/xinitrc.d ]; then
for f in /etc/X11/xinit/xinitrc.d/*; do
  [ -x "$f" ] && . "$f"
done
unset f
fi

if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi

/usr/bin/xdg-user-dirs-update &
setxkbmap -layout us,ru -variant -option grp:caps_toggle,terminate:ctrl_alt_bksp &
xsetroot -cursor_name left_ptr &
xset s 6999 &
xset dpms 6000 6001 6002 &
xset m 5/2 0 &
numlockx &
perWindowLayoutD &
compton -b &
urxvtd -q -f -o &
pcmanfm -d &
feh --bg-center /home/haron/pics/xmonad/XMONAD011-2.png &
# /usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --width 1 --transparent true --alpha 1 --tint 0x000000 --height 20 --expand true &
stalonetray &
python3 /home/haron/lib/gis-weather/gis-weather.py &
# mocicon &
exec xmonad

