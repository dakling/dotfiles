#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# History settings
shopt -s histappend
export HISTCONTROL=ignoredups:erasedups

# Basic aliases
alias ls='ls --color=auto'
alias ll='ls -lrth --color=auto'
alias ..='cd ..'
alias ...='cd ../..'
alias grep='grep --color=auto'
alias mkdir='mkdir -pv'

# Safety aliases
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'

# Editor aliases
alias ee='emacsclient -t -a nvim'
alias vim='nvim'
export EDITOR='nvim'

# Process monitoring
alias top='btm'
alias htop='btm'

# System update
alias update='sudo apt update && sudo apt upgrade'

alias xx='Xephyr -br -ac -noreset -screen 1920x1080 :1 & DISPLAY=:1'
alias fix_screen='~/.screenlayout/single.sh; ~/.screenlayout/default.sh ; ~/.screenlayout/default.sh'

# SSH mount aliases
mount_fairphone() { sshfs fairphone:/home/phablet ~/mnt/fairphone/; }
mount_nexus() { sshfs nexus:/home/phablet ~/mnt/nexus/; }
mount_purism() { sshfs purism:/home/purism ~/mnt/purism/; }
mount_pi() { sshfs pi:/home/klingenberg ~/mnt/pi/; }
liwi_ssh_tunnel() { ssh -f -N lichtwiese-tunnel; }
mount_liwi() { sshfs lichtwiese:/home/klingenberg ~/mnt/lichtwiese/; }
mount_liwivpn() { sshfs lichtwiesevpn:/home/klingenberg ~/mnt/lichtwiese/; }
mount_lcluster() { sshfs lcluster:/home/km88econ ~/mnt/lichtenberg/; }
mount_jenkins_old() { sshfs jenkins-old:/ ~/mnt/jenkins/; }
mount_cadmium() { sshfs cadmium:/home/dsk34/ ~/mnt/cadmium/; }
mount_fawcett() { sshfs fawcett:/home/dsk34/ ~/mnt/fawcett/; }
mount_maths() { sshfs maths:/home/dsk34/ ~/mnt/maths/; }
mount_swirles() { sshfs swirles:/cephfs/home/dsk34/ ~/mnt/swirles/; }
mount_store_swirles() { sshfs swirles:/cephfs/store/fluids-rrk26/dsk34/ ~/mnt/swirles_store/; }
mount_store_maths() { sshfs maths:/store/DAMTP/dsk34 ~/mnt/maths_store/; }
mount_data_maths() { sshfs maths:/data/septal/dsk34 ~/mnt/maths_data/; }
mount_wilkes() { sshfs wilkes:/home/dsk34/ ~/mnt/wilkes/; }
alias fe41='source /home/klingenberg/foam/foam-extend-4.1/etc/bashrc'

source ~/.bash_aliases.sh

function latexdiff-vc-most-recent(){
    latexdiff-vc -r HEAD^ -r HEAD "$1" --pdf
}

function scrcb() {
    scrot $1 -e 'xclip -selection clipboard -t image/png -i $f'
}

# Enhanced PS1 with git branch and colors
parse_git_branch() {
    git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
export PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\[\033[33m\]$(parse_git_branch)\[\033[00m\]\$ '

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

# Additional conda initialization
[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh
# <<< conda initialize <<<
