#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

alias ee='emacsclient -t -a nvim'
alias vim='emacsclient -t -a nvim'
alias nvim='emacsclient -t -a nvim'

alias top='btm'
alias htop='btm'

alias xx='Xephyr -br -ac -noreset -screen 1920x1080 :1 & DISPLAY=:1'
alias fix_screen='~/.screenlayout/single.sh; ~/.screenlayout/double.sh ; ~/.screenlayout/double.sh'

alias mount_fairphone='sshfs fairphone:/home/phablet ~/mnt/fairphone/'
alias mount_nexus='sshfs nexus:/home/phablet ~/mnt/nexus/'
alias mount_purism='sshfs purism:/home/purism ~/mnt/purism/'
alias liwi_ssh_tunnel='ssh -f -N lichtwiese-tunnel'
alias mount_liwi='sshfs lichtwiese:/home/klingenberg ~/mnt/lichtwiese/'
alias mount_liwivpn='sshfs lichtwiesevpn:/home/klingenberg ~/mnt/lichtwiese/'
alias mount_lcluster='sshfs lcluster:/home/km88econ ~/mnt/lichtenberg/'
alias mount_jenkins_old='sshfs jenkins-old:/ ~/mnt/jenkins/'
alias fe41='source /home/klingenberg/foam/foam-extend-4.1/etc/bashrc'

source ~/.bash_aliases.sh

function latexdiff-vc-most-recent(){
    latexdiff-vc -r HEAD^ -r HEAD "$1" --pdf
}

function scrcb() {
    scrot $1 -e 'xclip -selection clipboard -t image/png -i $f'
}

PS1='[\u@\h \W]\$ '

export PATH=~/.config/emacs/bin/:$PATH
export FOAM_DG_ROOT=~/Documents-work/programming/foam-dg/foam-dg/

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

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/usr/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/usr/etc/profile.d/conda.sh" ]; then
        . "/usr/etc/profile.d/conda.sh"
    else
        export PATH="/usr/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

