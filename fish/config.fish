# Files in conf.d can be overridden by the user
# by files with the same name in $XDG_CONFIG_HOME/fish/conf.d

# This file is run by all fish instances.
# To include configuration only for login shells, use
# if status --is-login
#    ...
# end
# To include configuration only for interactive shells, use
# if status --is-interactiv
#   ...
# end

# Start X at login
#if status --is-login
    #if test -z "$DISPLAY" -a $XDG_VTNR = 1
        #exec startx -- -keeptty
    #end
#end

fish_vi_key_bindings

#aliases
alias ... cd\ ../..
alias .... cd\ ../../..

alias nn nano
alias vi "nvim"
alias vim "nvim"
alias gnvim "nvim-qt"

alias tops "ps -A|grep"

alias lclusterlogin "ssh -YC km88econ@lcluster8.hrz.tu-darmstadt.de"
alias flolclusterlogin "ssh -YC mo52ixew@lcluster5.hrz.tu-darmstadt.de"
alias lclustermount "sshfs -o follow_symlinks km88econ@lcluster8.hrz.tu-darmstadt.de:/home/km88econ/ ~/.lclustermountpoint ; cd .lclustermountpoint"
alias flolclustermount "sshfs -o follow_symlinks mo52ixew@lcluster8.hrz.tu-darmstadt.de:/home/mo52ixew/ ~/.flolclustermountpoint ; cd .flolclustermountpoint"

#source ~/OpenFOAM/OpenFOAM-2.4.0/etc/bashrc

alias rr "ranger --choosedir=$HOME/.rangerdir; cd (cat $HOME/.rangerdir)"

export EDITOR="nvim"
export TERMCMD="xterm"
export TERMINAL="terminology"
export TERMINAL="terminology"
export TERM="xterm-256color"
