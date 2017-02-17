# Author - Haron Prime
# The following lines were added by compinstall
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit promptinit zfinit
compinit
promptinit
zfinit
# End of lines added by compinstall
ZSH=/usr/share/oh-my-zsh/
DISABLE_AUTO_UPDATE="true"
ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache
if [[ ! -d $ZSH_CACHE_DIR ]]; then
    mkdir $ZSH_CACHE_DIR
fi

case "$TERM" in
  'xterm') TERM=xterm-256color;;
  'screen') TERM=screen-256color;;
  'Eterm') TERM=Eterm-256color;;
esac

export LC_CTYPE="en_US.utf8"
export LANG="ru_UA.UTF-8"
export MM_CHARSET="UTF-8"
export PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/games:$HOME/.local/bin"
export PAGER="less"
export EDITOR="vim"
export ARCHFLAGS="arch x86_64"
export BROWSER="vivaldi-snapshot"
export MAIL="thunderbird"
export OOO_FORCE_DESKTOP="xmonad"
export MANOPT="-L ru"

setopt appendhistory autocd extendedglob nomatch
setopt IGNORE_EOF
setopt CORRECT_ALL
SPROMPT="Ошибка! ввести %r вместо %R? ([Y]es/[N]o/[E]dit/[A]bort) "
bindkey -e

autoload colors; colors
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
 
# prompt (if running screen, show window #)

 if [ x$WINDOW != x ]; then
     export PS1="%{$fg[white]%}┌─[%{$fg[green]%}%n@%m %B%{$fg[blue]%}%l %{$fg[red]%}%? %{$fg[blue]%}%h %b%{$fg[green]%}%d%{$fg[white]%}] %{$fg[cyan]%} %*%{$reset_color%}"$'\n'"%{$fg[white]%}└─> %{$reset_color%}"
 else
     export PS1="%{$fg[white]%}┌─[%{$fg[green]%}%n@%m %B%{$fg[blue]%}%l %{$fg[red]%}%? %{$fg[blue]%}%h %b%{$fg[green]%}%d%{$fg[white]%}] %{$fg[cyan]%} %*%{$reset_color%}"$'\n'"%{$fg[white]%}└─> %{$reset_color%}"
 fi
 export RPROMPT="$GIT_PROMPT%{$reset_color%}"

# format titles for screen and rxvt
function title() {
  # escape '%' chars in $1, make nonprintables visible
  a=${(V)1//\%/\%\%}
 
  # Truncate command, and join lines.
  a=$(print -Pn "%40>...>$a" | tr -d "\n")
 
  case $TERM in
  screen*)
    print -Pn "\ek$a:$3\e\\" # screen title (in ^A")
    ;;
  xterm*|rxvt*)
    precmd() { print -Pn "\e]0;%m:%~\a" }
    preexec () { print -Pn "\e]0;$1\a" }
    ;;
  esac
}
 
# precmd is called just before the prompt is printed
function precmd() {
  title "zsh" "$USER@%m" "%55<...<%~"
}
 
# preexec is called just before any command line is executed
function preexec() {
  title "$1" "$USER@%m" "%35<...<%~"
}

setopt menucomplete
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path $HOME/.zsh/cache
zstyle ':completion:*' insert-tab false
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \ 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=$color[cyan]=$color[red]"

# syntax-highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
ZSH_HIGHLIGHT_STYLES[default]='none'
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES=(
        'alias'                  'fg=cyan'
        'builtin'                'fg=yellow'
        'function'               'fg=blue,bold'
        'command'                'fg=green'
        'hashed-commands'        'fg=green,underline'
        'precommand'             'fg=cyan,bold'
        'commandseparator'       'fg=yellow'
        'assign'                 'fg=magenta'
        'path'                   'underline'
        'double-hyphen-option'   'fg=blue,bold'
        'single-hyphen-option'   'fg=blue,bold'
        'unknown-token'          'fg=red'
)
ZSH_HIGHLIGHT_STYLES[bracket-level-1]='fg=blue,bold'
ZSH_HIGHLIGHT_STYLES[bracket-level-2]='fg=red,bold'
ZSH_HIGHLIGHT_STYLES[bracket-level-3]='fg=yellow,bold'
ZSH_HIGHLIGHT_STYLES[bracket-level-4]='fg=magenta,bold'
ZSH_HIGHLIGHT_PATTERNS+=('sudo' 'fg=red,bold')
ZSH_HIGHLIGHT_STYLES[root]='bg=red'

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

[[ -a $(whence -p pacman-color) ]] && compdef _pacman pacman-color=pacman

# active plugins
source /usr/share/oh-my-zsh/plugins/archlinux/archlinux.plugin.zsh
source /usr/share/oh-my-zsh/plugins/autojump/autojump.plugin.zsh
source /usr/share/oh-my-zsh/plugins/command-not-found/command-not-found.plugin.zsh
source /usr/share/oh-my-zsh/plugins/cp/cp.plugin.zsh
source /usr/share/oh-my-zsh/plugins/rsync/rsync.plugin.zsh
source /usr/share/oh-my-zsh/plugins/web-search/web-search.plugin.zsh
source /usr/share/oh-my-zsh/plugins/history-substring-search/history-substring-search.zsh
source /usr/share/oh-my-zsh/plugins/zsh_reload/zsh_reload.plugin.zsh
source /usr/share/oh-my-zsh/plugins/colorize/colorize.plugin.zsh
source $HOME/scripts/my_zhs_plugins/global_aliases.plugin.zsh
source $HOME/scripts/my_zhs_plugins/my_aliases.plugin.zsh
source $HOME/scripts/my_zhs_plugins/extract.plugin.zsh
source $HOME/scripts/my_zhs_plugins/pk.plugin.zsh
source $HOME/scripts/my_zhs_plugins/git-prompt.plugin.zsh
source $HOME/scripts/my_zhs_plugins/zsh-autosuggestions.plugin.zsh

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

##Set some keybindings
#################################################
bindkey '^[[A' up-line-or-search                # up arrow for back-history-search
bindkey '^[[B' down-line-or-search              # down arrow for fwd-history-search
bindkey ';5D' backward-word                     # ctrl+left 
bindkey ';5C' forward-word                      # ctrl+right
bindkey '\e[1~' beginning-of-line               # home
bindkey '\e[2~' overwrite-mode                  # insert
bindkey '\e[3~' delete-char                     # del
bindkey '\e[4~' end-of-line                     # end
bindkey '\e[5~' up-line-or-history              # page-up
bindkey '\e[6~' down-line-or-history            # page-down
#################################################

#  History
export HISTFILE=$HOME/.zsh_history 
export SAVEHIST=$HISTSIZE
export HISTSIZE=10000
setopt APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt INC_APPEND_HISTORY
setopt SH_WORD_SPLIT
setopt AUTO_CD

autoload -U run-help
autoload run-help-git
autoload run-help-svn
autoload run-help-svk
alias help=run-help

autoload -U edit-command-line

# colorize man-page
man() {
      env \
          LESS_TERMCAP_mb=$(printf "\e[1;31m") \
          LESS_TERMCAP_md=$(printf "\e[1;31m") \
          LESS_TERMCAP_me=$(printf "\e[0m") \
          LESS_TERMCAP_se=$(printf "\e[0m") \
          LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
          LESS_TERMCAP_ue=$(printf "\e[0m") \
          LESS_TERMCAP_us=$(printf "\e[1;32m") \
          PAGER=/usr/bin/less \
          _NROFF_U=1 \
          PATH=${HOME}/.local/bin:${PATH} \
             man "$@"
}

DIRSTACKSIZE=20

setopt autopushd pushdsilent pushdtohome

if [ -f $HOME/scripts/s-completion.bash ]; then
    . $HOME/scripts/s-completion.bash
fi
