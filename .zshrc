#!/usr/bin/env zsh
# Author - Haron Prime
# License - WTFPL http://www.wtfpl.net/ 

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
export PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/games:/usr/bin/core_perl:/usr/lib/ccache/bin/:$HOME/.local/bin:$HOME/.cabal/bin"
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

export PS1="%{$fg[white]%}┌─[%{$fg[green]%}%n %l %{$fg[red]%}%? %{$fg[green]%}%d%{$fg[white]%}] %{$fg_bold[blue]%}%*%{$reset_color%}"$'\n'"%{$fg[white]%}└─> %{$reset_color%}"

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
zstyle ':completion:*' use-cache true
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


[[ -a $(whence -p pacman-color) ]] && compdef _pacman pacman-color=pacman

# active plugins
# source $HOME/scripts/my_zhs_plugins/archlinux.plugin.zsh
source $HOME/scripts/my_zhs_plugins/autojump.plugin.zsh
source $HOME/scripts/my_zhs_plugins/command-not-found.plugin.zsh
source $HOME/scripts/my_zhs_plugins/cp.plugin.zsh
source $HOME/scripts/my_zhs_plugins/rsync.plugin.zsh
source $HOME/scripts/my_zhs_plugins/web-search.plugin.zsh
source $HOME/scripts/my_zhs_plugins/history-substring-search.zsh
source $HOME/scripts/my_zhs_plugins/zsh_reload.plugin.zsh
source $HOME/scripts/my_zhs_plugins/colorize.plugin.zsh
source $HOME/scripts/my_zhs_plugins/action.plugin.zsh
source $HOME/scripts/my_zhs_plugins/aliases.plugin.zsh
source $HOME/scripts/my_zhs_plugins/aurman.plugin.zsh
source $HOME/scripts/my_zhs_plugins/git-prompt.plugin.zsh
source $HOME/scripts/my_zhs_plugins/zsh-autosuggestions.plugin.zsh
source $HOME/scripts/my_zhs_plugins/git-flow-completion.plugin.zsh
source $HOME/scripts/my_zhs_plugins/grc-aliases.plugin.zsh
source $HOME/scripts/my_zhs_plugins/yay.plugin.zsh
# source $HOME/git/zsh-git-prompt/zshrc.sh

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
# man() {
#       env \
#           LESS_TERMCAP_mb=$(printf "\e[1;31m") \
#           LESS_TERMCAP_md=$(printf "\e[1;31m") \
#           LESS_TERMCAP_me=$(printf "\e[0m") \
#           LESS_TERMCAP_se=$(printf "\e[0m") \
#           LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
#           LESS_TERMCAP_ue=$(printf "\e[0m") \
#           LESS_TERMCAP_us=$(printf "\e[1;32m") \
#           PAGER=/usr/bin/less \
#           _NROFF_U=1 \
#           PATH=${HOME}/.local/bin:${PATH} \
#              man "$@"
# }

DIRSTACKSIZE=20

setopt autopushd pushdsilent pushdtohome

# if [ -f $HOME/scripts/s-completion.bash ]; then
#     . $HOME/scripts/s-completion.bash
# fi

# fasd_cache="$HOME/.fasd-init-zsh"
# if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
#   fasd --init posix-alias zsh-hook zsh-ccomp zsh-ccomp-install zsh-wcomp zsh-wcomp-install >| "$fasd_cache"
# fi
# source "$fasd_cache"
# unset fasd_cache

