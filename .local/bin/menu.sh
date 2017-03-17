#!/bin/zsh

category=$(echo -e "Development\nGraphics\nMultimedia
Network\nTorrent\nOffice\nApplications
Settings\nLogout" | dmenu -i -sb '#333' -nf '#999' -sf '#9df' -fn 'Terminus Re33:size=12' -p Category)

case $category in

        Development) proglist="Meld
Sublime-text-3
XVim" ;;

        Graphics) proglist="Gimp
Gpick
Inkscape
Mirage" ;;

        Multimedia) proglist="Vlc
Sopcast-player
DeadBeef
EasyTags" ;;

        Network) proglist="Vivaldi-snapshot
Firefox
Thunderbird
Hexchat
Viber" ;;

        Torrent) proglist="Transmission-daemon
TransGUI" ;;

        Office) proglist="LibreOffice" ;;

        Applications) proglist="Shutter" ;;

        Settings) proglist="LXAppearance
Qt4-config" ;;

        Logout) proglist="XKill
Reboot
Poweroff" ;;
esac

program=$(echo -e "$proglist" | dmenu -i -sb '#333' -nf '#999' -sf '#9df' -fn 'Terminus Re33:size=12' -p $category)
case $program in

        Meld) meld ;;
        Sublime-text-3) subl3 ;;
        XVim) xvim ;;

        Gimp) gimp-2.8 ;;
        Gpick) gpick ;;
        Inkscape) inkscape ;;
        Mirage) mirage ;;

        Vlc) vlc ;;
        Sopcast-player) sopcast-player ;;
        DeadBeef) deadbeef ;;
        EasyTag) easytag ;;

        Vivaldi-snapshot) vivaldi-snapshot ;;
        Firefox) firefox ;;
        Thunderbird) thunderbird ;;
        Hexchat) hexchat ;;
        Viber) viber ;;

        Transmission-daemon) transd ;;
        TransGUI) transgui ;;

        LibreOffice) soffice ;;

        Shutter) shutter ;;

        LXAppearance) lxappearance ;;
        Qt4-config) qtconfig-qt4 ;;

        XKill) killx ;;
        Reboot) compreboot ;;
        Poweroff) compdown ;;
esac