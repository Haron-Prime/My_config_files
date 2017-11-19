# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
export PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/games:$HOME/.local/bin:$HOME/.cabal/bin"
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
  alias configure='grc --colour=auto configure'
  alias ping="grc --colour=auto ping -4"
  alias traceroute="grc --colour=auto traceroute"
  alias gcc="grc --colour=auto gcc"
  alias make="grc --colour=auto make"
  alias netstat="grc --colour=auto netstat"
  alias stat="grc --colour=auto stat"
  alias ss="grc --colour=auto ss"
  alias diff="grc --colour=auto diff"
  alias wdiff="grc --colour=auto wdiff"
  alias last='grc --colour=auto last'
  alias who='grc --colour=auto who'
  alias mount="grc --colour=auto mount"
  alias mtr="grc --colour=auto mtr"
  alias ps="grc --colour=auto ps"
  alias dig="grc --colour=auto dig"
  alias ifconfig='grc --colour=auto ifconfig'
  alias ls="grc --colour=auto ls"
  alias df="grc --colour=auto df"
  alias du="grc --colour=auto du"
  alias addr="grc --colour=auto ip addr"
  alias link="grc --colour=auto ip link"
  alias route="grc --colour=auto ip route"
  alias neighbor='grc --colour=ip neighbor'
  alias env="grc --colour=auto env"
  alias iptables='sudo grc --colour=auto iptables'
  alias lspci='grc --colour=auto lspci'
  alias lsblk='grc --colour=auto lsblk'
  alias blkid='grc --colour=auto blkid'
  alias iostat='grc --colour=auto iostat'
  alias iostat_sar="grc --colour=auto iostat_sar"
  alias sar='grc --colour=auto sar'
  alias free='grc --colour=auto free'
  alias journalctl='grc --colour=auto journalctl'
  alias sysctl='grc --colour=auto sysctl'
  alias systemctl='grc --colour=auto systemctl'
  alias tcpdump='grc --colour=auto tcpdump'
  alias tune2fs='grc --colour=auto tune2fs'
  alias lsmod='grc --colour=auto lsmod'
  alias lsattr='grc --colour=auto lsattr'
  alias semanage='grc --colour=auto semanage'
  alias getsebool='grc --colour=auto getsebool'
  alias unlimit='grc --colour=auto unlimit'
  alias vnstat="grc --colour=auto vnstat"
  alias dnf='grc --colour=auto dnf'
  alias nmap='grc --colour=auto nmap'
  alias uptime='grc --colour=auto uptime'
  alias w='grc --colour=auto w'
  alias getfacl='grc --colour=auto getfacl'
  alias logc="grc --colour=auto cat"
  alias logt="grc --colour=auto tail"
  alias logh="grc --colour=auto head"
  alias ant="grc --colour=auto ant"
  alias fdisk="grc --colour=auto fdisk"
  alias findmnt="grc --colour=auto findmnt"
  alias id="grc --colour=auto id"
  alias ip="grc --colour=auto ip"
  alias ipaddr="grc --colour=auto ipaddr"
  alias ipneighbor="grc --colour=auto ipneighbor"
  alias iproute="grc --colour=auto iproute"
  alias lsof="grc --colour=auto lsof"
  alias mvn="grc --colour=auto mvn"
  alias pv="grc --colour=auto pv"
  alias semanageboolean="grc --colour=auto semanageboolean"
  alias semanagefcontext="grc --colour=auto semanagefcontext"
  alias semanageuser="grc --colour=auto semanageuser"
  alias showmount="grc --colour=auto showmount"
  alias ulimit="grc --colour=auto ulimit"
  alias vmstat="grc --colour=auto vmstat"
fi

# my aliases
alias x='exit'
alias ipt='iptables -L -vn'
alias dump='sudo screendump > ~/output.txt'
# alias gmx='mount https://storage-file-eu.gmx.com'
alias nout='ssh haron@192.168.0.11 -p 232'
alias tv='mpv ~/Video/multinet-play.m3u'
alias st='subl3'
alias sst='sudo subl3'
alias vnspeed='vnstat -l -i enp3s0'
alias nstat="netstat -atpn"
alias fonts='fc-cache -fv ~/.fonts && sudo fc-cache -fv'
alias wget='wget -c --progress=bar'
alias nano='nano -w'
alias dirf='find . -type d | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/"'
alias ttmux='tmuxinator default'
alias tmuxa='tmux attach'
# alias mirrorlist='sudo reflector --verbose -l 5 --sort rate --save /etc/pacman.d/mirrorlist'
alias psc='ps xawf -eo pid,user,cgroup,args'
alias gzip='tar -cvzf'
alias bzip='tar -cvjf'
# alias mfs='sudo mhddfs /media/haron/BKP,/media/haron/Data /mnt/all -o default_permissions,allow_other'
alias sctl='systemctl'
alias jctl='journalctl'
alias jctlc='sudo journalctl --vacuum-size=20000'
alias jctlb='journalctl -b'
alias jctlf='journalctl -f'
alias jctlerr='journalctl -b -p3 g -v transmission'
alias rein='sudo renice -5 $(pidof -s Xorg)'
alias svim='sudo vim'
alias cclean='yaourt -Sc'
alias pclean='sudo pacman -Rns $(pacman -Qtdq)'
alias pacfs='pacman -Fs'
alias sv='s -b vivaldi-snapshot'
alias asl='acestream-launcher --player mpv'
alias xx='startx ~/.xinitrc-xmonad'
alias xh='startx ~/.xinitrc-herbstluftwm'
alias xi='startx ~/.xinitrc-i3'
alias xa='startx ~/.xinitrc-awesome'
alias mcf='cd ~/git/My_config_files'
alias gitf='git add --all && git commit -m "Update" && git push'
alias gita='git add'
alias giti='git add -i'
alias gitc='git commit -m "Update"'
alias gitp='git push'
alias gits='git status'
alias gitl='git log'

# other aliases
alias ls="ls -la --classify --color=auto"
alias grep='grep --colour=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias li='ls -ial'

alias mv='nocorrect mv -i'
alias cp='nocorrect cp -iR'
alias rm='nocorrect rm -i'
alias rmr='nocorrect rm -iR'
alias rmf='nocorrect rm -if'
alias rmrf='nocorrect rm -fR'
alias mkdir='nocorrect mkdir'

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

PS1='\[\033[00m\]┌─ \[\033[01;32m\]\u  `pwd`  \[\033[00m\]$(date +%H:%M:%S)\[\033[00m\]
└─> \[\033[01;32m\]$\[\033[00m\] '


[ -f ~/.fzf.bash ] && source ~/.fzf.bash
