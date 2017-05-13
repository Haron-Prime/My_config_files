#!/usr/bin/env zsh
# Author Haron Prime
# License - © 2017 WTFPL - http://www.wtfpl.net/ 

# мои альясы
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
alias jctlerr='journalctl -b -p3 g'
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
alias mysync='rsync -ruo --exclude ".git" --progress /home/haron/git/My_config_files /home/haron/git/BitBuket/my_config_files'


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
  # alias warn="grc -c conf.warn cat"

fi

# глобальные альясы zsh
# global aliases
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g CA="2>&1 | cat -A"
alias -g C='| wc -l'
alias -g D="DISPLAY=:0.0"
alias -g DN=/dev/null
alias -g ED="export DISPLAY=:0.0"
alias -g EG='|& egrep'
alias -g EH='|& head'
alias -g EL='|& less'
alias -g ELS='|& less -S'
alias -g ETL='|& tail -20'
alias -g ET='|& tail'
alias -g F=' | fmt -'
alias -g G='| egrep'
alias -g H='| head'
alias -g HL='|& head -20'
alias -g Sk="*~(*.bz2|*.gz|*.tgz|*.zip|*.z)"
alias -g LL="2>&1 | less"
alias -g L="| less"
alias -g LS='| less -S'
alias -g MM='| most'
alias -g M='| more'
alias -g NE="2> /dev/null"
alias -g NS='| sort -n'
alias -g NUL="> /dev/null 2>&1"
alias -g PIPE='|'
alias -g R=' > /c/aaa/tee.txt '
alias -g RNS='| sort -nr'
alias -g S='| sort'
alias -g TL='| tail -20'
alias -g T='| tail'
alias -g US='| sort -u'
alias -g VM='/var/log/messages'
alias -g X0G='| xargs -0 egrep'
alias -g X0='| xargs -0'
alias -g XG='| xargs egrep'
alias -g X='| xargs'
alias -g g="| grep"
alias -g l="| less"
alias -g C='| wc -l'

# другие альясы
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

alias -s {avi,mpeg,mpg,mov,m2v,mkv}=mpv
alias -s {odt,doc,sxw,rtf}=libreoffice
alias -s {png,gif,jpg,jpeg}=feh
alias -s text=vim
