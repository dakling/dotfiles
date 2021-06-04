#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

alias ee='emacsclient -t -a vim'
alias vim='emacsclient -t -a vim'
alias nvim='emacsclient -t -a vim'

PS1='[\u@\h \W]\$ '

export PATH=~/.config/emacs/bin/:$PATH

# up-down arrow to search in history
bind '"\e[A": history-substring-search-backward'
bind '"\e[B": history-substring-search-forward'
