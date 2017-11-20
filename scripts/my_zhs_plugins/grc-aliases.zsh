#!/usr/bin/env zsh
# Author Haron Prime
# License - © 2017 WTFPL - http://www.wtfpl.net/ 

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
