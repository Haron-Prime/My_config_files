#!/usr/bin/env sh
PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/games:$HOME/.local/bin:$HOME/.cabal/bin"
SHELL="/bin/sh"

export LANG=ru_UA.UTF-8
export MM_CHARSET=UTF-8
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export FT2_SUBPIXEL_HINTING=2
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export LIBVA_DRIVER_NAME=vdpau
export VDPAU_DRIVER=nouveau

# if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
#   exec startx
# fi
