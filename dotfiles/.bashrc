# ------------------------------------------------------------------------------
# --------------------------------- ~/.bashrc ----------------------------------
# ------------------------------------------------------------------------------
# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# ------------------------------------------------------------------------------
# ------------------------------ Utility scripts -------------------------------
# ------------------------------------------------------------------------------
dotfiles=$HOME/dotfiles
determ_platform=$dotfiles/utils/determ_platform.sh
exist_command=$dotfiles/utils/exist_command.sh
git_prompt=$dotfiles/utils/git/git-prompt.sh
git_completion=$dotfiles/utils/git/git-completion.bash

# determine the platform
platform=`$determ_platform`

# ------------------------------------------------------------------------------
# --------------------------- Linux Default Settings ---------------------------
# ------------------------------------------------------------------------------
# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
      . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
      . /etc/bash_completion
    fi
fi

# ------------------------------------------------------------------------------
# ------------------------------- Bash Settings --------------------------------
# ------------------------------------------------------------------------------
# Bash base settings
shopt -s cdspell       # auto fix cd path
shopt -s dirspell      # auto fix dir path
shopt -s histappend    # append to the history file, don't overwrite it
shopt -s checkwinsize  # check the window size after each command
shopt -s globstar      # enable **
shopt -s autocd        # enable change directory without `cd`

# Bash key bindings
bind '"\C-j": menu-complete'
bind '"\C-k": menu-complete-backward'
# bind '"\C-j": history-search-backward'
# bind '"\C-k": history-search-forward'
bind '"\C-l": forward-char'
bind '"\C-h": backward-char'
bind '"\C-f": forward-word'
bind '"\C-b": backward-word'
bind '"\C-w": unix-filename-rubout'
bind 'TAB: menu-complete'
bind '"\e[Z": menu-complete-backward'

# Bash additional settings
bind 'set show-all-if-ambiguous on'
bind 'set show-all-if-unmodified on'
bind 'set completion-ignore-case on'
bind 'set completion-map-case off'
bind 'set menu-complete-display-prefix on'
bind 'set skip-completed-text on'
bind 'set visible-stats on'
bind 'set colored-stats on'
bind 'set colored-completion-prefix on'
bind 'set bell-style none'    # Disable beep
bind "set bind-tty-special-chars off"  # No overwrite of Ctrl-W

# ------------------------------------------------------------------------------
# ------------------------------- Basic Aliases --------------------------------
# ------------------------------------------------------------------------------
# Enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    # alias dir='dir --color=auto'
    # alias vdir='vdir --color=auto'

    alias grep='grep --color=auto -i'
    alias fgrep='fgrep --color=auto -i'
    alias egrep='egrep --color=auto -i'
fi

# Aliases for ls
alias ll='ls -alF'
alias la='ls -A'
alias l='ls'
alias sl=ls

# Aliases for cd
function cd {
    if [ $# -eq 0 ]; then
        builtin cd ~ && ls;
    else
        pushd "$@" && ls;
    fi
}
function pushd() { command pushd "$@" > /dev/null; }  # silent `pushd`
function popd() { command popd "$@" > /dev/null; }    # silent `popd`
alias dirs='dirs -v'  # enumerating directory stack with numbers
alias d=dirs
for i in {0..10}; do
    alias "$i"="cd +$i"
    alias cd"$i"="cd +$i"
done
alias c=cd
alias cd-="cd -"
alias cd..="cd .."
alias cd...="cd ../.."
alias cd....="cd ../../.."
alias cd.....="cd ../../../.."
alias cd......="cd ../../../../.."
alias c-="cd -"
alias c..="cd .."
alias c...="cd ../.."
alias c...="cd ../../.."
alias c....="cd ../../../.."
alias c.....="cd ../../../../.."

# Aliases for clear
alias cl=clear

# Alart (ex: `sleep 10; alert`)
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Horizontal line
function hline() { printf -- "─%.0s" $(seq $(tput cols)); }
# Color
function color_f() { printf "$(tput setaf $1)"; }
function color_b() { printf "$(tput setab $1)"; }
function color_fb() { printf "$(color_f $1)$(color_b $2)"; }
function color_F() { printf "$(tput bold)$(color_f $1)"; }
function color_FB() { printf "$(tput bold)$(color_fb $1 $2)"; }
function color_end() { printf "$(tput sgr0)"; }

# ------------------------------------------------------------------------------
# ----------------------------------- Prompt -----------------------------------
# ------------------------------------------------------------------------------
# PS1 styles
PS1_PREFIX=""
function set_ps1_pure() {
    # ```takiyu ~/dotfiles (master>) $ ```
    local DEBIAN_CHROOT='${debian_chroot:+$debian_chroot }'
    local USER='\[$(tput bold)$(tput setaf 2)\]\u'
    local DIRNAME='\[$(tput setaf 4)\]\w'
    local GIT='\[$(tput setaf 1)\]$(__git_ps1)'
    local PROMPT='\[$(tput setaf 4)\$\]'
    local END='\[$(color_end)\]'
    PS1="$PS1_PREFIX$DEBIAN_CHROOT$USER $DIRNAME$GIT $PROMPT $END"
}
function set_ps1_rich() {
    # ``` takiyu  ~/dotfiles  master=  ```
    ps1_mode=$1
    BG_COL_1=110  # 149
    BG_COL_2=241
    BG_COL_3=239
    FG_COL_1=16  # 0
    FG_COL_2=7
    FG_COL_3=1  # 176
    local DEBIAN_CHROOT='${debian_chroot:+$debian_chroot }'
    local USER='\[$(color_fb $FG_COL_1 $BG_COL_1)\] \u'
    local DIRNAME='\[$(color_FB $FG_COL_2 $BG_COL_2)\] \w'
    local END='\[$(color_end)\]'
    if [ "$ps1_mode" == 0 ]; then
        # Full
        # ``` takiyu  ~/dotfiles  master=  ```
        local SEP1='\[$(color_FB $BG_COL_1 $BG_COL_2)\]'
        local SEP2='\[$(color_FB $BG_COL_2 $BG_COL_3)\]'
        local GIT='\[$(color_FB $FG_COL_3 $BG_COL_3)\]$(__git_ps1 " %s ")'
        local SEP3='\[$(color_end)$(color_F $BG_COL_3)\]'
        PS1="$PS1_PREFIX$DEBIAN_CHROOT$USER $SEP1$DIRNAME $SEP2$GIT$SEP3$END "
    elif [ "$ps1_mode" == 1 ]; then
        # No git
        # ``` takiyu  ~/dotfiles  ```
        local SEP1='\[$(color_FB $BG_COL_1 $BG_COL_2)\]'
        local SEP2='\[$(color_end)$(color_F $BG_COL_2)\]'
        PS1="$PS1_PREFIX$DEBIAN_CHROOT$USER $SEP1$DIRNAME $SEP2$END "
    else
        # No git. No font
        # ``` takiyu  ~/dotfiles  ```
        PS1="$PS1_PREFIX$DEBIAN_CHROOT$USER $DIRNAME $END "
    fi
}

# Set PS1
PROMPT_DIRTRIM=2
source $git_completion
if [ $platform == 'Linux' ]; then
    GIT_PS1_SHOWUPSTREAM=1
    GIT_PS1_SHOWUNTRACKEDFILES=
    GIT_PS1_SHOWSTASHSTATE=
    GIT_PS1_SHOWDIRTYSTATE=
    source $git_prompt
    # Color prompt (with git and font)
    set_ps1_rich 0
elif [ $platform == 'Windows' ]; then
    # Color prompt (without git, with font)
    set_ps1_rich 1
fi
# Set prompt at the bottom
# PS1='\[$(tput cup "$LINES")\]'$PS1

# ------------------------------------------------------------------------------
# ------------------------------- Command Hooks --------------------------------
# ------------------------------------------------------------------------------
# Previous
function pre_cmd_handler() {
    # Escape non-after post_cmd_handler
    if [ -z "$__cmd_handler_post" ]; then return; fi
    __cmd_handler_post=

    # Empty command handling
    if [ "$BASH_COMMAND" == "post_cmd_handler" ]; then
        # dirs  # Print directory stack
        ls
        __cmd_handler_empty_cnt=$((__cmd_handler_empty_cnt+1))
    else
        __cmd_handler_empty_cnt=
    fi
    __cmd_handler_pre=1
}
trap "pre_cmd_handler" DEBUG

# Post
function post_cmd_handler() {
    # Capture exit code
    __prev_exit_code=$?

    # With non-empty command
    if [ -z "$__cmd_handler_empty_cnt" ]; then
        # Print exit code
        if [  $__prev_exit_code != 0 ]; then
            echo "$(color_FB 7 1) [Exit code: $__prev_exit_code] $(color_end)"
        fi
    fi

    # Escape initial prompt
    # if [ -n "$__cmd_handler_pre" ]; then
    #     # Print per-prompt spacing
    #     # hline
    #     echo ''
    # fi

    # With empty command
    if [ -n "$__cmd_handler_empty_cnt" ]; then
        # Clear with continuous Enter
        if [ 2 -le $__cmd_handler_empty_cnt ]; then
            clear -x
            __cmd_handler_empty_cnt=
        fi
    fi

    __cmd_handler_post=1
}
PROMPT_COMMAND="post_cmd_handler"

# ------------------------------------------------------------------------------
# --------------------------------- Git Alias ----------------------------------
# ------------------------------------------------------------------------------
alias g='git'
alias ginit='git init && git commit --allow-empty -m "First commit"'
alias gs='git status'
function gg() { git graph --color=always $* | less -EFRSX; }
function gl() { git log --color=always --graph $* | less -EFRX; }
function gla() { git log --color=always --graph --all $* | less -EFRX; }
alias gb='git branch'
alias gbd='git branch -D'
alias gbD='git branch -D'
alias gba='git branch -a'
alias gbc='git branch --show-current'
alias gurl='git remote -v'
alias gd='git diff'
alias gdc='git diff --cached'
alias ga='git add'
alias ga.='git add .'
alias gau='git add -u'
alias gaup='git add -u -p'
alias gcm='git commit'
function gcmm() { git commit -m "$*"; }
alias gcma='git commit --amend'
function gcmma() { git commit --amend -m "$*"; }
function gcmam() { git commit --amend -m "$*"; }
alias gclo='git clone'
alias gcl='git clean -i'
alias gco='git checkout'
alias gcob='git checkout -b'
function gcof() { b="$*"; git branch -d $b && git checkout -b $b; }
function gcoF() { b="$*"; git branch -D $b; git checkout -b $b; }
alias gm='git merge'
alias grb='git rebase'
alias gf='git fetch -p'
alias gfr='git fetch -p --recurse-submodules -j8'
alias gp='git pull'
alias gP='git push'
alias gpo='git pull origin'
alias gPo='git push origin'
alias gpoc='git pull origin `gbc`'
alias gPoc='git push origin `gbc`'
function gPoA() { git push origin :"$*"; git push origin "$*"; }
function gPoR() { git push origin :"$*"; git push origin "$*"; }
function gPoAc() { git push origin :`gbc`; git push origin `gbc`; }
function gPoRc() { git push origin :`gbc`; git push origin `gbc`; }
function gPocA() { git push origin :`gbc`; git push origin `gbc`; }
function gPocR() { git push origin :`gbc`; git push origin `gbc`; }
function gPoD() { git push origin :"$*"; }
alias gPocD='git push origin :`gbc`'
alias gpom='git pull origin master'
alias gPom='git push origin master'
alias gr='git reset'
alias gR='git reset --hard'
function gro() { git reset origin/"$*"; }
function gRo() { git reset origin/"$*" --hard; }
alias grom='git reset origin/master'
alias gRom='git reset --hard origin/master'
alias gst='git stash'
alias gstl='git stash list'
alias gstp='git stash pop'
alias gsta='git stash apply'
alias gstc='git stash clear'
alias gsts='git stash show -p'
alias gsub='git submodule'
alias gsb='git submodule'
alias gsuburi='git submodule update --init --recursive -j 8'
alias gsburi='git submodule update --init --recursive -j 8'
alias gbis='git bisect'
function gbis_start() { git bisect start HEAD "$1"; }  # $1: bad
alias gbis_run_script='git bisect run'
function gbis_start_with_script() { gbis_start "$1" && gbis_run_script "$2"; }
alias gbis_mark_good='git bisect good'
alias gbis_mark_bad='git bisect bad'
alias gbis_reset='git bisect reset'
alias gbis_exit=gbis_reset
alias gbis_log='git bisect log'
alias gbis_view='git bisect view'

# ------------------------------------------------------------------------------
# ------------------------------- Git Completion -------------------------------
# ------------------------------------------------------------------------------
__git_complete g __git_main
__git_complete gs _git_status
__git_complete gg _git_log
__git_complete gl _git_log
__git_complete gla _git_log
__git_complete gb _git_branch
__git_complete gbd _git_branch
__git_complete gbD _git_branch
__git_complete gba _git_branch
__git_complete gurl __git_remotes
__git_complete gd _git_diff
__git_complete gdc _git_diff
__git_complete ga _git_add
__git_complete gau _git_add
__git_complete gaup _git_add
__git_complete gcm _git_commit
__git_complete gcmm _git_commit
__git_complete gcma _git_commit
__git_complete gcmma _git_commit
__git_complete gcmam _git_commit
__git_complete gclo _git_clone
__git_complete gco _git_checkout
__git_complete gcob _git_checkout
__git_complete gcof _git_checkout
__git_complete gcoF _git_checkout
__git_complete gm _git_merge
__git_complete grb _git_rebase
__git_complete gf _git_fetch
__git_complete gfr _git_fetch
__git_complete gp _git_pull
__git_complete gP _git_push
__git_complete gpo _git_branch
__git_complete gPo _git_branch
__git_complete gPoA _git_branch
__git_complete gPoR _git_branch
__git_complete gPoD _git_branch
__git_complete gr _git_reset
__git_complete gR _git_reset
__git_complete gro _git_branch
__git_complete gRo _git_branch
__git_complete gs _git_stash
__git_complete gsub _git_submodule
__git_complete gsb _git_submodule
__git_complete gbis _git_bisect
__git_complete gbis_start _git_branch
__git_complete gbis_start_with_script _git_branch

# ------------------------------------------------------------------------------
# ----------------------------- Additional Aliases -----------------------------
# ------------------------------------------------------------------------------
# aliases for make
alias maek=make
alias mkae=make
alias mkea=make
alias meak=make
alias meka=make
alias amke=make
alias amek=make
alias akme=make
alias akem=make
alias aemk=make
alias aekm=make
alias kmae=make
alias kmea=make
alias kame=make
alias kaem=make
alias kema=make
alias keam=make
alias emak=make
alias emka=make
alias eamk=make
alias eakm=make
alias ekma=make
alias ekam=make
alias mk=make
alias km=make
# aliases for ninja
alias nin=ninja
alias ni=ninja

# aliases for editors
if [ "`$exist_command nvim`" == 'exist' ]; then
    alias vim=nvim
fi
if [ $platform == 'Linux' ]; then
    if [ "`$exist_command nvim-qt`" == 'exist' ]; then
        function gvim() { command nvim-qt $@ 2> /dev/null; }
    fi
elif [ $platform == 'Windows' ]; then
    if [ "`$exist_command nvim-qt`" == 'exist' ]; then
        function gvim() { command nvim-qt $@ & 2> /dev/null; disown; }
    fi
fi
alias v="gvim"
alias gv="gvim"
alias vimdiff="vim -d"
alias gvimdiff="gvim -- -d"
function gvim_nofork() { command nvim-qt --nofork $@ 2> /dev/null; }
alias gvimdiff_nofork="gvim_nofork -- -d"
export EDITOR=gvim
export GIT_EDITOR="nvim-qt --nofork"  # Blocking command

# aliases for fzy
function gvimf() { gvim `find | fzy`; }
function cdf() { cd `find | fzy`; }
function lsf() { find | fzy; }

# aliases for applications
if [ $platform == 'Linux' ]; then
    function filer() { command thunar $@ & 2> /dev/null; disown; }
    function zathura() { command zathura $@ & 2> /dev/null; disown; }
elif [ $platform == 'Windows' ]; then
    function filer() { command explorer $@ & 2> /dev/null; disown; }
    alias w=winpty
fi
alias f=filer
alias f.='filer .'
alias f..='filer ..'

# Trizen
if [ "`$exist_command trizen`" == 'exist' ]; then
    alias trizen-noconfirm="trizen -Syu -y --noconfirm"
fi

# Synaptics and Game pad
if [ $platform == 'Linux' ]; then
    if [ "`$exist_command synclient`" == 'exist' ]; then
        # bg for speed up
        function synclient_bg() { command synclient $@ & 2> /dev/null; disown; }
        synclient_bg VertScrollDelta=-30 HorizScrollDelta=-30
        synclient_bg MaxSpeed=2.0 AccelFactor=0.10
    fi
    export SDL_JOYSTICK_DEVICE=/dev/input/js0
    export SDL_GAMECONTROLLERCONFIG='030000008f0e00000300000010010000,GreenAsia Inc.    USB Joystick,platform:Linux,a:b1,b:b2,x:b3,y:b0,back:b8,start:b9,leftstick:b10,rightstick:b11,leftshoulder:b6,rightshoulder:b7,dpup:h0.1,dpdown:h0.4,dpleft:h0.8,dpright:h0.2,leftx:a0,lefty:a1,rightx:a3,righty:a2,lefttrigger:b4,righttrigger:b5,'  # Generated by `controllermap 0`
fi

# ------------------------------------------------------------------------------
# ----------------------------------- Proxy ------------------------------------
# ------------------------------------------------------------------------------
# export PROXY_USER=''
# export PROXY_PASS=''
# export PROXY_MODE=''
if [ "$PROXY_MODE" == 'Huawei_linux' ]; then
    export PROXY_HOST='proxyjp.huawei.com:8080'
    # PROXY_HOST='localhost:8888'
    export HTTP_PROXY="http://$PROXY_USER:$PROXY_PASS@$PROXY_HOST"
    export HTTPS_PROXY="$HTTP_PROXY"
    export FTP_PROXY="$HTTP_PROXY"
    export NO_PROXY="huawei.com,localhost"
    export GIT_SSL_NO_VERIFY=1
    export CURL_SSL_NO_VERIFY=1
fi

export http_proxy=$HTTP_PROXY
export https_proxy=$HTTPS_PROXY
export ftp_proxy=$FTP_PROXY
export no_proxy=$NO_PROXY

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
