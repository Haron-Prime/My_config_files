#!/bin/zsh

category=$(echo -e "Development\nGraphics\nMedia\nNetwork\nOffice\nWork\nTools\nSettings\nLogout" | dmenu -i -sb '#333' -nf '#999' -sf '#9df' -fn 'Terminus Re33:size=12' -p Category)

case $category in

        Development) proglist="Meld\nSublime-text-3\nXVim" ;;

        Graphics) proglist="Gimp\nGpick\nInkscape\nMirage" ;;

        Media) proglist="Vlc\nSopcast-player\nDeadBeef\nEasyTags" ;;

        Network) proglist="Vivaldi-snapshot\nFirefox\nThunderbird\nTransGUI\nTransmission-daemon\nTixati\nHexchat\nViber\nZenmap" ;;

        Office) proglist="LibreOffice\nFoxitReader" ;;

        Work) proglist="PRO100\n2D-Place" ;;

        Tools) proglist="Shutter\nSystemadm\nCryptkeeper\nGparted\nGSmartControl\nPacmanXG" ;;

        Settings) proglist="LXAppearance\nQt4-config\nOomox\nDconf-editor" ;;

        Logout) proglist="XKill\nReboot\nPoweroff" ;;
esac

program=$(echo -e "$proglist" | dmenu -i -sb '#333' -nf '#999' -sf '#9df' -fn 'Terminus Re33:size=12' -p $category)
case $program in
# Development
        Meld) meld ;;
        Sublime-text-3) subl3 ;;
        XVim) xvim ;;
# Graphics
        Gimp) gimp-2.8 ;;
        Gpick) gpick ;;
        Inkscape) inkscape ;;
        Mirage) mirage ;;
# Multimedia
        Vlc) vlc ;;
        Sopcast-player) sopcast-player ;;
        DeadBeef) deadbeef ;;
        EasyTag) easytag ;;
# Network
        Vivaldi-snapshot) vivaldi-snapshot ;;
        Firefox) firefox ;;
        Thunderbird) thunderbird ;;
        Hexchat) hexchat ;;
        Viber) viber ;;
        Transmission-daemon) transd ;;
        TransGUI) transgui ;;
        Tixati) tixati ;;
        Zenmap) zenmap ;;
# Office
        LibreOffice) soffice ;;
        FoxitReader) FoxitReader ;;
# Work
        PRO100) PRO100-5 ;;
        2D-Place) 2D-Place ;;
# Tools
        Shutter) shutter ;;
        Systemadm) systemadm ;;
        Cryptkeeper) cryptkeeper ;;
        Gparted) gksu gparted ;;
        GsmartControl) gsmartcontrol ;;
        PacmanXG) ssx pacmanxg ;;
# Settings
        LXAppearance) lxappearance ;;
        Qt4-config) qtconfig-qt4 ;;
        Oomox) oomox-gui ;;
        Dconf-editor) dconf-editor ;;
# Logout
        XKill) killx ;;
        Reboot) compreboot ;;
        Poweroff) compdown ;;
esac