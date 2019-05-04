[ -r /etc/profile.d/cnf.sh ] && . /etc/profile.d/cnf.sh


## Options section
setopt correct                                                  # Auto correct mistakes
setopt extendedglob                                             # Extended globbing. Allows using regular expressions with *
setopt nocaseglob                                               # Case insensitive globbing
setopt rcexpandparam                                            # Array expension with parameters
setopt nocheckjobs                                              # Don't warn about running processes when exiting
setopt numericglobsort                                          # Sort filenames numerically when it makes sense
setopt nobeep                                                   # No beep
setopt appendhistory                                            # Immediately append history instead of overwriting
setopt histignorealldups                                        # If a new command is a duplicate, remove the older one
setopt autocd                                                   # if only directory path is entered, cd there.

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'       # Case insensitive tab completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' rehash true                              # automatically find new executables in path 
# Speed up completions
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
HISTFILE=~/.zhistory
HISTSIZE=1000
SAVEHIST=500
setopt histignorespace
export EDITOR=/usr/bin/nvim
export VISUAL=/usr/bin/nvim
export PAGER=/usr/bin/nvimpager
export QT_QPA_PLATFORMTHEME="qt5ct"
#wayland stuff
# export QT_QPA_PLATFORM=wayland-egl
# export GDK_BACKEND=wayland
# export GTK_CSD=0
# source $HOME/OpenFOAM/OpenFOAM-dev/etc/bashrc
export BOSSS_INSTALL=/home/klingenberg/BoSSS-experimental
export LatexGlobalConfig=/home/klingenberg/Documents/programming/latex/definGlobal.tex
export BIBLIOGRAPHY=/home/klingenberg/Documents/programming/latex/bibliography.bib
export TERMINAL=termite
export PATH="$PATH:/home/klingenberg/.stack/"
export PATH="$PATH:/home/klingenberg/.local/bin/"
export PATH="$PATH:/home/klingenberg/.cabal/bin/"
MAIL=$HOME/mail/inbox && export MAIL
WORDCHARS=${WORDCHARS//\/[&.;]}                                 # Don't consider certain characters part of the word
# export RoslynTargetsPath=/usr/lib/mono/msbuild/15.0/bin/  
# export BOSSS_INSTALL=/home/klingenberg/BoSSS-experimental
# BoSSS


 ##Keybindings section
case ${TERM} in
  *termite)
    zle-keymap-select () {
    if [ $KEYMAP = vicmd ]; then
        printf "\033[2 q"
    else
        printf "\033[6 q"
    fi
    }
    zle -N zle-keymap-select
    zle-line-init () {
    zle -K viins
   printf "\033[6 q"
    }
    zle -N zle-line-init
    zle -N zle-keymap-select
    ;;
esac
bindkey -v
# Remove delay when entering normal mode (vi)
KEYTIMEOUT=5
#bindkey '^[[7~' beginning-of-line                               # Home key
#bindkey '^[[8~' end-of-line                                     # End key
#if [[ "${terminfo[khome]}" != "" ]]; then
  #bindkey "${terminfo[khome]}" beginning-of-line                # [Home] - Go to beginning of line
#fi
#if [[ "${terminfo[kend]}" != "" ]]; then
#  bindkey "${terminfo[kend]}" end-of-line                      # [End] - Go to end of line
#fi
#bindkey '^[[2~' overwrite-mode                                  # Insert key
#bindkey '^[[3~' delete-char                                     # Delete key
#bindkey '^[[C'  forward-char                                    # Right key
#bindkey '^[[D'  backward-char                                   # Left key
#bindkey '^[[5~' history-beginning-search-backward               # Page up key
#bindkey '^[[6~' history-beginning-search-forward                # Page down key

# Navigate words with ctrl+arrow keys
#bindkey '^[Oc' forward-word                                     #
#bindkey '^[Od' backward-word                                    #
#bindkey '^[[1;5D' backward-word                                 #
#bindkey '^[[1;5C' forward-word                                  #
#bindkey '^H' backward-kill-word                                 # delete previous word with ctrl+backspace
#bindkey '^[[Z' undo                                             # Shift+tab undo last action

## Alias section 
alias ls="ls --color"
alias ...="cd ../.."
alias ....="cd ../../.."

# Theming section  
autoload -U compinit colors zcalc
compinit -d
colors

# enable substitution for prompt
setopt prompt_subst

# Prompt (on left side) similar to default bash prompt, or redhat zsh prompt with colors
 #PROMPT="%(!.%{$fg[red]%}[%n@%m %1~]%{$reset_color%}# .%{$fg[green]%}[%n@%m %1~]%{$reset_color%}$ "
# Maia prompt
PROMPT="%B%{$fg[cyan]%}%(4~|%-1~/.../%2~|%~)%u%b >%{$fg[cyan]%}>%B%(?.%{$fg[cyan]%}.%{$fg[red]%})>%{$reset_color%}%b " # Print some system information when the shell is first started
# Print a greeting message when shell is started
echo $USER@$HOST  $(uname -srm) $(lsb_release -rcs)
## Prompt on right side:
#  - shows status of git when in git repository (code adapted from https://techanic.net/2012/12/30/my_git_prompt_for_zsh.html)
#  - shows exit status of previous command (if previous command finished with an error)
#  - is invisible, if neither is the case

# Modify the colors and symbols in these variables as desired.
GIT_PROMPT_SYMBOL="%{$fg[blue]%}±"                              # plus/minus     - clean repo
GIT_PROMPT_PREFIX="%{$fg[green]%}[%{$reset_color%}"
GIT_PROMPT_SUFFIX="%{$fg[green]%}]%{$reset_color%}"
GIT_PROMPT_AHEAD="%{$fg[red]%}ANUM%{$reset_color%}"             # A"NUM"         - ahead by "NUM" commits
GIT_PROMPT_BEHIND="%{$fg[cyan]%}BNUM%{$reset_color%}"           # B"NUM"         - behind by "NUM" commits
GIT_PROMPT_MERGING="%{$fg_bold[magenta]%}⚡︎%{$reset_color%}"     # lightning bolt - merge conflict
GIT_PROMPT_UNTRACKED="%{$fg_bold[red]%}●%{$reset_color%}"       # red circle     - untracked files
GIT_PROMPT_MODIFIED="%{$fg_bold[yellow]%}●%{$reset_color%}"     # yellow circle  - tracked files modified
GIT_PROMPT_STAGED="%{$fg_bold[green]%}●%{$reset_color%}"        # green circle   - staged changes present = ready for "git push"

parse_git_branch() {
  # Show Git branch/tag, or name-rev if on detached head
  ( git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD ) 2> /dev/null
}

parse_git_state() {
  # Show different symbols as appropriate for various Git repository states
  # Compose this value via multiple conditional appends.
  local GIT_STATE=""
  local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_AHEAD" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD}
  fi
  local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_BEHIND" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND}
  fi
  local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
  if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
  fi
  if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_UNTRACKED
  fi
  if ! git diff --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MODIFIED
  fi
  if ! git diff --cached --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_STAGED
  fi
  if [[ -n $GIT_STATE ]]; then
    echo "$GIT_PROMPT_PREFIX$GIT_STATE$GIT_PROMPT_SUFFIX"
  fi
}

git_prompt_string() {
  local git_where="$(parse_git_branch)"
  
  # If inside a Git repository, print its branch and state
  [ -n "$git_where" ] && echo "$GIT_PROMPT_SYMBOL$(parse_git_state)$GIT_PROMPT_PREFIX%{$fg[yellow]%}${git_where#(refs/heads/|tags/)}$GIT_PROMPT_SUFFIX"
  
  # If not inside the Git repo, print exit codes of last command (only if it failed)
  [ ! -n "$git_where" ] && echo "%{$fg[red]%} %(?..[%?])"
}

# Right prompt with exit status of previous command if not successful
 #RPROMPT="%{$fg[red]%} %(?..[%?])" 
# Right prompt with exit status of previous command marked with ✓ or ✗
 #RPROMPT="%(?.%{$fg[green]%}✓ %{$reset_color%}.%{$fg[red]%}✗ %{$reset_color%})"


# Color man pages
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-r


## Plugins section: Enable fish style features
# Use syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# Use history substring search
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
# bind UP and DOWN arrow keys to history substring search
zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey '^[[A' history-substring-search-up			
bindkey '^[[B' history-substring-search-down

case ${TERM} in
  linux)
    RPROMPT="%{$fg[red]%} %(?..[%?])" 
    alias x='startx ~/.xinitrc'      # Type name of desired desktop after x, xinitrc is configured for it
    ;;
  xterm)
    RPROMPT='$(git_prompt_string)'
    ;;
  rxvt*)
    RPROMPT='$(git_prompt_string)'
	### Base16 Shell color themes.
	##possible themes: 3024, apathy, ashes, atelierdune, atelierforest, atelierhearth,
	##atelierseaside, bespin, brewer, chalk, codeschool, colors, default, eighties, 
	##embers, flat, google, grayscale, greenscreen, harmonic16, isotope, londontube,
	##marrakesh, mocha, monokai, ocean, paraiso, pop (dark only), railscasts, shapesifter,
	##solarized, summerfruit, tomorrow, twilight
	#theme="tomorrow"
	##Possible variants: dark and light
	#shade="dark"
	#BASE16_SHELL="/usr/share/zsh/scripts/base16-shell/base16-$theme.$shade.sh"
	#[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL
    # Use autosuggestion
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
    ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
    ;;  
  *)
    RPROMPT='$(git_prompt_string)'
	### Base16 Shell color themes.
	##possible themes: 3024, apathy, ashes, atelierdune, atelierforest, atelierhearth,
	##atelierseaside, bespin, brewer, chalk, codeschool, colors, default, eighties, 
	##embers, flat, google, grayscale, greenscreen, harmonic16, isotope, londontube,
	##marrakesh, mocha, monokai, ocean, paraiso, pop (dark only), railscasts, shapesifter,
	##solarized, summerfruit, tomorrow, twilight
	#theme="monokai"
	##Possible variants: dark and light
	#shade="dark"
	#BASE16_SHELL="/usr/share/zsh/scripts/base16-shell/base16-$theme.$shade.sh"
	#[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL
    # Use autosuggestion
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
    ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=7'
    ;;
esac

function qmount {
    if [ $1 = 'lectures' ] 
    then
        sudo mount //dc1/misc/fdy-lectures.git ~/git/mnt/fdy-lectures.git -t cifs -o username=klingenberg,noexec,uid=klingenberg
    elif [ $1 = 'klausuren' ]
    then
        sudo mount //dc1/lehre/TM1/Klausuren.git ~/git/mnt/Klausuren.git -t cifs -o username=klingenberg,noexec,uid=klingenberg
    elif [ $1 = 'misc' ]
    then
        sudo mount //dc1/misc ~/misc -t cifs -o username=klingenberg,noexec,uid=klingenberg
    elif [ $1 = 'publications' ]
    then
        sudo mount //dc1/misc/fdy-publications.git ~/git/mnt/fdy-publications.git -t cifs -o username=klingenberg,noexec,uid=klingenberg
    elif [ $1 = 'scratch' ]
    then
        sudo mount //dc1/scratch/ ~/scratch -t cifs -o username=klingenberg,noexec,uid=klingenberg
    elif [ $1 = 'lehre' ]
    then
        sudo mount //dc1/lehre/ ~/lehre -t cifs -o username=klingenberg,noexec,uid=klingenberg
    else
    echo "$1 not known"
    fi
}

function applyOften {
    for file in "${@:2:$#}";
    do 
        eval "$1" $file;
    done
}

source /usr/share/fzf/completion.zsh && source /usr/share/fzf/key-bindings.zsh
_fzf_compgen_path() {
  fd --hidden --follow . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow . "$1"
}
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export PATH="/home/klingenberg/.guix-profile/bin${PATH:+:}$PATH"
