# Section: Bootstrap {{{
export TERM=xterm-256color
export LANG=en_IN
export CDPATH=".:$DOT:$HOME/coding:$HOME/work"
export PATH="$HOME/.local/scripts:$PATH"
export HISTSIZE=1000
export HISTFILESIZE=2000
export HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help"
export EDITOR="vim"
if [[ $TMUX_PANE ]]; then
    export HISTFILE=$HOME/.bash_history_tmux_${TMUX_PANE:1}
fi
# }}}
# Section: Alias {{{
unalias -a

alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

alias tree='tree -a'
alias tree2='tree -a -L 2'
alias tree3='tree -a -L 3'
alias tree4='tree -a -L 4'

alias ls='ls -h --color=auto'
alias l="ls -lah"
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias gdb="gdb -q"
alias teh="tmux-here"

alias gs='git s'
alias gl='git l'
alias gb='git b'
alias update='sudo apt-get update && sudo apt-get upgrade'
# }}}
[[ -r "$HOME/.bashrc.local" ]] && source "$HOME/.bashrc.local"
#  vi:set ts=2 sts=2 sw=2 et fdm=marker:
