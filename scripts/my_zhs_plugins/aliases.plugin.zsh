#!/bin/zsh
# Author Haron Prime
# License - © 2017 WTFPL - http://www.wtfpl.net/ 

# мои альясы
# my aliases
alias x='exit'
alias ipt='iptables -L -vn'
alias dump='sudo screendump > ~/output.txt'
# alias gmx='mount https://storage-file-eu.gmx.com'
alias st='subl3'
alias sst='sudo subl3'
alias vnspeed='vnstat -l -i enp3s0'
alias nstat="netstat -atpn"
alias fonts='fc-cache -fv ~/.fonts && sudo fc-cache -fv'
alias wget='wget -c --progress=bar'
alias nano='nano -w'
alias mv='nocorrect mv -i'
alias cp='nocorrect cp -iR'
alias rm='nocorrect rm -i'
alias rmr='nocorrect rm -iR'
alias rmf='nocorrect rm -if'
alias rmrf='nocorrect rm -fR'
alias mkdir='nocorrect mkdir'
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
alias rein='sudo renice -5 $(pidof -s Xorg)'
alias svim='sudo vim'
alias yaclean='yaourt -Sc'
alias pclean='sudo pacman -Rns $(pacman -Qtdq)'
alias pacfs='pacman -Fs'
alias s='s -b vivaldi-snapshot'
alias asl='acestream-launcher --player mpv'
alias xx='startx ~/.xinitrc-xmonad'
alias xh='startx ~/.xinitrc-herbstluftwm'
alias xi='startx ~/.xinitrc-i3'
alias xa='startx ~/.xinitrc-awesome'
alias xs='startx ~/.xinitrc-spectrwm'
alias gitf='git add --all && git commit -m "Update" && git push'
alias gita='git add'
alias giti='git add -i'
alias gitc='git commit -m "Update"'
alias gitp='git push'
alias gits='git status'
alias nout='ssh haron@192.168.0.11 -p 232'
alias tv='mpv ~/Video/multinet-play.m3u'

# Распаковка архивов
# example: extract file
extract () {
 if [ -f $1 ] ; then
  case $1 in
  *.tar.bz2)   tar xjf $1        ;;
  *.tar.gz)    tar xzf $1     ;;
  *.bz2)       bunzip2 $1       ;;
  *.rar)       unrar x $1     ;;
  *.gz)        gunzip $1     ;;
  *.tar)       tar xf $1        ;;
  *.tbz2)      tar xjf $1      ;;
  *.tbz)       tar -xjvf $1    ;;
  *.tgz)       tar xzf $1       ;;
  *.zip)       unzip $1     ;;
  *.Z)         uncompress $1  ;;
  *.7z)        7z x $1    ;;
  *)           echo "I don't know how to extract '$1'..." ;;
  esac
 else
  echo "'$1' is not a valid file"
 fi
}

# Запаковать архив
# example: pk tar file
pk () {
 if [ $1 ] ; then
  case $1 in
  tbz)       tar cjvf $2.tar.bz2 $2      ;;
  tgz)       tar czvf $2.tar.gz  $2       ;;
  tar)       tar cpvf $2.tar  $2       ;;
  bz2)       bzip $2 ;;
  gz)        gzip -c -9 -n $2 > $2.gz ;;
  zip)       zip -r $2.zip $2   ;;
  7z)        7z a $2.7z $2    ;;
  *)         echo "'$1' cannot be packed via pk()" ;;
  esac
 else
  echo "'$1' is not a valid file"
 fi

} 

# цветной вывод с помощью grc
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
  alias sar='grc --colour=auto sar'
  alias free='grc --colour=auto free'

  alias journalctl='grc --colour=auto journalctl'
  alias sysctl='grc --colour=auto sysctl'
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

  alias warn="grc -c conf.warn cat"
fi

# aliases
alias ls="ls -la --classify --color=auto"
alias grep='grep --colour=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias li='ls -ial'

alias -s {avi,mpeg,mpg,mov,m2v,mkv}=mpv
alias -s {odt,doc,sxw,rtf}=libreoffice
alias -s {png,gif,jpg,jpeg}=feh
alias -s text=vim
