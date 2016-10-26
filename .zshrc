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
# export TERM='xterm-256color'
# [ -n "$XTERM_VERSION" ] && transset-df -a >/dev/null

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
 # export RPROMPT="$GIT_PROMPT%{$reset_color%}"

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

# If not running interactively, do not do anything
# [[ $- != *i* ]] && return
# [[ -z "$TMUX" ]] && exec mux default

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
  alias ping="grc --colour=auto ping -4"
  alias traceroute="grc --colour=auto traceroute"
  alias make="grc --colour=auto make"
  alias diff="grc --colour=auto diff"
  alias cvs="grc --colour=auto cvs"
  alias netstat="grc --colour=auto netstat"
  alias nmap='grc --colour=auto nmap'
  alias logc="grc --colour=auto cat"
  alias logt="grc --colour=auto tail"
  alias logh="grc --colour=auto head"
fi

[[ -a $(whence -p pacman-color) ]] && compdef _pacman pacman-color=pacman

# active plugins
# source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/oh-my-zsh/plugins/archlinux/archlinux.plugin.zsh
source /usr/share/oh-my-zsh/plugins/autojump/autojump.plugin.zsh
source /usr/share/oh-my-zsh/plugins/command-not-found/command-not-found.plugin.zsh
source /usr/share/oh-my-zsh/plugins/cp/cp.plugin.zsh
# source /usr/share/oh-my-zsh/plugins/github/github.plugin.zsh
source /usr/share/oh-my-zsh/plugins/rsync/rsync.plugin.zsh
source /usr/share/oh-my-zsh/plugins/web-search/web-search.plugin.zsh
source /usr/share/oh-my-zsh/plugins/history-substring-search/history-substring-search.zsh
source /usr/share/oh-my-zsh/plugins/zsh_reload/zsh_reload.plugin.zsh
source /usr/share/oh-my-zsh/plugins/colorize/colorize.plugin.zsh
# source $HOME/scripts/my_zhs_plugins/git-prompt/git-prompt.plugin.zsh
source $HOME/scripts/my_zhs_plugins/global_aliases.plugin.zsh
source $HOME/scripts/my_zhs_plugins/my_aliases.plugin.zsh
source $HOME/scripts/my_zhs_plugins/extract.plugin.zsh
source $HOME/scripts/my_zhs_plugins/pk.plugin.zsh
source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source /home/haron/scripts/my_zhs_plugins/git-prompt.zsh
 
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
 
export LC_CTYPE="en_US.utf8"
export PATH=$HOME/.local/bin:/usr/local/bin:$PATH
export PAGER="less"
export EDITOR="vim"
export ARCHFLAGS="arch x86_64"
export BROWSER="vivaldi-snapshot"
export MAIL="thunderbird"
export OOO_FORCE_DESKTOP="xmonad"


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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# DIRSTACKFILE=$HOME/.zsh/cache-dirs/dirs"
# if [[ -f $DIRSTACKFILE  ]] && [[ $#dirstack -eq 0  ]]; then
#      dirstack=( ${(f)"$(< $DIRSTACKFILE)"}  )
# 	   [[ -d $dirstack[1]  ]] && cd $dirstack[1]
#	fi
#	chpwd() {
#	     print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
#
#	}

DIRSTACKSIZE=20

setopt autopushd pushdsilent pushdtohome

# extract () {
#   if [ -f $1 ] ; then
#     case $1 in
#       *.tar.bz2) tar xvjf $1   ;;
#       *.tar.gz)  tar xvzf $1   ;;
#       *.tar.xz)  tar xvfJ $1   ;;
#       *.bz2)     bunzip2 $1    ;;
#       *.rar)     unrar x $1    ;;
#       *.gz)      gunzip $1     ;;
#       *.tar)     tar xvf $1    ;;
#       *.tbz2)    tar xvjf $1   ;;
#       *.tgz)     tar xvzf $1   ;;
#       *.zip)     unzip $1      ;;
#       *.Z)       uncompress $1 ;;
#       *.7z)      7z x $1       ;;
#       *)         echo "'$1' cannot be extracted via >extract<" ;;
#     esac
#   else
#     echo "'$1' is not a valid file"
#   fi
# }

if [ -f $HOME/scripts/s-completion.bash ]; then
    . $HOME/scripts/s-completion.bash
fi
