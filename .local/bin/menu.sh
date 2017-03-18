#!/bin/zsh

category=$(echo -e "Files\nDevelopment\nGraphics\nMedia\nNetwork\nGames\nOffice\nWork\nTools\nSettings\nLogout" | dmenu -i -sb '#333' -nf '#999' -sf '#9df' -fn 'Terminus Re33:size=12' -p Category)

case $category in

        Files) proglist="/\nHome\nMCF\nDocuments\nDownloads\nMusic\nPictures\nProjects\nPublic\nTemplates\nVideo" ;;

        Development) proglist="Meld\nSublime-text-3\nXVim\nXMicro" ;;

        Graphics) proglist="Gimp\nGpick\nInkscape\nMirage" ;;

        Media) proglist="Vlc\nSopcast-player\nDeadBeef\nEasyTags\nPulseAudio" ;;

        Network) proglist="Vivaldi-snapshot\nFirefox\nThunderbird\nTransGUI\nTransmission-daemon\nTixati\nHexchat\nTelegram\nViber\nZenmap" ;;

        Games) proglist="HMM3\nStarMade" ;;

        Office) proglist="LibreOffice\nFoxitReader" ;;

        Work) proglist="PRO100\n2D-Place" ;;

        Tools) proglist="Shutter\nHtop\nMC\nRanger\nSystemadm\nCryptkeeper\nGparted\nGSmartControl\nPacmanXG" ;;

        Settings) proglist="LXAppearance\nQt4-config\nOomox\nDconf-editor" ;;

        Logout) proglist="XKill\nReboot\nPoweroff" ;;
esac

program=$(echo -e "$proglist" | dmenu -i -sb '#333' -nf '#999' -sf '#9df' -fn 'Terminus Re33:size=12' -p $category)
case $program in
# Files
        /) xdg-open / ;;
        Home) xdg-open ~/ ;;
        MCF) xdg-open ~/git/My_config_files ;;
        Documents) xdg-open ~/Documents ;;
        Downloads) xdg-open ~/Downloads ;;
        Music) xdg-open ~/Music ;;
        Pictures) xdg-open ~/Pictures ;;
        Projects) xdg-open ~/Projects ;;
        Public) xdg-open ~/Public ;;
        Templates) xdg-open ~/Templates ;;
        Video) xdg-open ~/Video ;;
# Development
        Meld) meld ;;
        Sublime-text-3) subl3 ;;
        XVim) xvim ;;
        XMicro) xmicro ;;
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
        PulseAudio) pavucontrol ;;
# Network
        Vivaldi-snapshot) vivaldi-snapshot ;;
        Firefox) firefox ;;
        Thunderbird) thunderbird ;;
        Hexchat) hexchat ;;
        Telegram) xdg-open https://web.telegram.org/#/im ;;
        Viber) viber ;;
        Transmission-daemon) transd ;;
        TransGUI) transgui ;;
        Tixati) tixati ;;
        Zenmap) gksu zenmap ;;
# Games
        HMM3) heroes3 ;;
        SarMade) starmade ;;
# Office
        LibreOffice) soffice ;;
        FoxitReader) FoxitReader ;;
# Work
        PRO100) PRO100-5 ;;
        2D-Place) 2D-Place ;;
# Tools
        Shutter) shutter ;;
        Htop) urxvtc -name htop -e htop ;;
        MC) urxvtc -name mc -e mc ;;
        Ranger) urxvtc -name ranger -e ranger ;;
        Systemadm) systemadm ;;
        Cryptkeeper) cryptkeeper ;;
        Gparted) gksu gparted ;;
        GSmartControl) gksu gsmartcontrol ;;
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