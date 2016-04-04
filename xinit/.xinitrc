#!/bin/sh
export LANG=ru_UA.UTF-8
export MM_CHARSET=UTF-8

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
perWindowLayoutD &
compton -b &
/home/haron/.load-xmonad
# /home/haron/.load-awesome

