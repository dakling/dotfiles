#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

alias ee='emacsclient -t -a nvim'
alias vim='emacsclient -t -a nvim'
alias nvim='emacsclient -t -a nvim'

function latexdiff-vc-most-recent(){
    latexdiff-vc -r HEAD^ -r HEAD "$1" --pdf
}

PS1='[\u@\h \W]\$ '

export PATH=~/.config/emacs/bin/:$PATH

# up-down arrow to search in history
bind '"\e[A": history-substring-search-backward'
bind '"\e[B": history-substring-search-forward'

alias hostname="echo $HOSTNAME"

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
fi

# vterm_printf(){
#     if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
#         # Tell tmux to pass the escape sequences through
#         printf "\ePtmux;\e\e]%s\007\e\\" "$1"
#     elif [ "${TERM%%-*}" = "screen" ]; then
#         # GNU screen (screen, screen-256color, screen-256color-bce)
#         printf "\eP\e]%s\007\e\\" "$1"
#     else
#         printf "\e]%s\e\\" "$1"
#     fi
# }

# if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
#     function clear(){
#         vterm_printf "51;Evterm-clear-scrollback";
#         tput clear;
#     }
# fi

# PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}:${PWD}\007"'

# vterm_cmd() {
#     local vterm_elisp
#     vterm_elisp=""
#     while [ $# -gt 0 ]; do
#         vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
#         shift
#     done
#     vterm_printf "51;E$vterm_elisp"
# }

# # should be at the end
# vterm_prompt_end(){
#     vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
# }
# PS1=$PS1'\[$(vterm_prompt_end)\]'
