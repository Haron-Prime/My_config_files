# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
export EDITOR=/usr/bin/vim
# xhost + >/dev/null
[ -n "$XTERM_VERSION" ] && transset-df -a >/dev/null
# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# usage grc
if [ -f /usr/bin/grc ]; then
  alias ping="grc --colour=auto ping"
  alias traceroute="grc --colour=auto traceroute"
  alias make="grc --colour=auto make"
  alias diff="grc --colour=auto diff"
  alias cvs="grc --colour=auto cvs"
  alias netstat="grc --colour=auto netstat"
  alias nmap='grc nmap'
  alias logc="grc cat"
  alias logt="grc tail"
  alias logh="grc head"
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# other aliases
alias x='exit'
alias UPD='yaourt -Syy'
alias UPG='yaourt -Syyua'
alias INST='yaourt -S'
alias DEL='yaourt -R'
alias inst='sudo pacman -Up'
alias clean='sudo pacman -Sc'
alias pacmmanxg='sudo pacmanxg'
alias SEARCH='yaourt -Ss'
alias IPT='sudo iptables -L -vn | less'
alias NTP='sudo ntpdate pool.ntp.org'
alias DUMP='sudo screendump > ~/output.txt'
alias RADIO='~/scripts/radio'
alias GMX='mount https://storage-file-eu.gmx.com'
alias ST='subl3'
alias SST='gksu subl3'
alias Asus='sshfs root@192.168.0.191:/ /media/haron/Asus'
alias NB='sshfs haron@192.168.0.103:/ /media/haron/Notebook'
alias NOUT='ssh haron@192.168.0.103'
alias NET='sudo sysctl -w net.ipv4.tcp_max_syn_backlog=9999'
alias TRIM='sudo fstrim -v / && sudo fstrim -v /home'
alias speed='vnstat -l -i enp3s0'
#alias rb='sudo shutdown -r now'
#alias stop='sudo shutdown -h now'
#alias F2B='sudo tail /var/log/fail2ban.log'
#alias slog='journalctl -b | grep sshd'
#alias blacklist="journalctl -b | grep sshd | grep 'Invalid user' | cut -d' ' -f10 | sort | uniq"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
alias rm='rm -I'


lightblue='\e[1;34m'
lightgreen='\e[1;32m'
lightred='\e[1;31m'

PS1='\[\033[00m\]┌─ \[\033[01;32m\]\u \[\033[00;36m\]{`uname -smr`}\[\033[01;32m\] `pwd` \[\033[00;31m\][\[\033[00m\]$(date +%H:%M:%S)\033[00;31m\]]\[\033[00m\]
└─> \[\033[01;32m\]$\[\033[00m\] '


[ -f ~/.fzf.bash ] && source ~/.fzf.bash
