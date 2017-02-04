#
# ~/.bash_profile
#

PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/games:$HOME/.local/bin"
SHELL="/bin/bash"

[[ -f ~/.bashrc ]] && . ~/.bashrc

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export TERM=xterm-256color
export LESS_TERMCAP_mb=$'\033[01;31m'
export LESS_TERMCAP_md=$'\033[01;31m'
export LESS_TERMCAP_me=$'\033[0m'
export LESS_TERMCAP_se=$'\033[0m'
export LESS_TERMCAP_so=$'\033[01;44;33m'
export LESS_TERMCAP_ue=$'\033[0m'
export LESS_TERMCAP_us=$'\033[01;32m'

# envfile="$HOME/.gnupg/gpg-agent.env"
# if [[ -e "$envfile" ]] && kill -0 $(grep GPG_AGENT_INFO "$envfile" | cut -d: -f 2) 2>/dev/null; then
#     eval "$(cat "$envfile")"
# else
#     eval "$(gpg-agent --daemon --enable-ssh-support --write-env-file "$envfile")"
# fi
# export GPG_AGENT_INFO  # the env file does not contain the export statement
# export SSH_AUTH_SOCK   # enable gpg-agent for ssh

export LANG=ru_UA.UTF-8
export MM_CHARSET=UTF-8
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export FT2_SUBPIXEL_HINTING=2
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export LIBVA_DRIVER_NAME=vdpau
export VDPAU_DRIVER=nouveau
