#
# ~/.bashrc: executed by bash(1) for non-login shells.
#

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# utility scripts
dotfiles=$HOME/dotfiles
determ_platform=$dotfiles/utils/determ_platform.sh
exist_command=$dotfiles/utils/exist_command.sh
git_prompt=$dotfiles/utils/git/git-prompt.sh
git_completion=$dotfiles/utils/git/git-completion.bash

# determine the platform
platform=`$determ_platform`

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

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

shopt -s cdspell       # auto fix cd path
shopt -s dirspell      # auto fix dir path
shopt -s histappend    # append to the history file, don't overwrite it
shopt -s checkwinsize  # check the window size after each command
shopt -s globstar      # enable **
shopt -s autocd        # enable change directory without `cd`
set bell-style none    # Disable beep

# bash key bindings
bind '"\C-j": menu-complete'
bind '"\C-k": menu-complete-backward'
# bind '"\C-j": history-search-backward'
# bind '"\C-k": history-search-forward'
bind '"\C-l": forward-char'
bind '"\C-h": backward-char'
bind '"\C-f": forward-word'
bind '"\C-b": backward-word'

# git prompt
source $git_completion
if [ $platform == 'Linux' ]; then
    GIT_PS1_SHOWUPSTREAM=1
    GIT_PS1_SHOWUNTRACKEDFILES=
    GIT_PS1_SHOWSTASHSTATE=
    GIT_PS1_SHOWDIRTYSTATE=
    source $git_prompt
    # color prompt
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u\[\033[01;34m\] \w\[\033[01;31m\]$(__git_ps1) \[\033[01;34m\]\$ \[\033[00m\]'
elif [ $platform == 'Windows' ]; then
    # color prompt
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u\[\033[01;34m\] \w \[\033[01;34m\]\$ \[\033[00m\]'
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto -i'
    alias fgrep='fgrep --color=auto -i'
    alias egrep='egrep --color=auto -i'
fi

# aliases for ls
alias ll='ls -alF'
alias la='ls -A'
alias l='ls'
alias sl=ls

# aliases for cd
alias cd-="cd -"
function cd {
    if [ $# -eq 0 ]; then
        builtin cd ~ && ls;
    else
        pushd "$@" && ls;
    fi
}
pushd () { command pushd "$@" > /dev/null; }  # silent `pushd`
popd () { command popd "$@" > /dev/null; }    # silent `popd`
alias dirs='dirs -v'  # enumerating directory stack with numbers
alias d=dirs
alias c=cd
for i in {0..10}; do
    alias "$i"="cd +$i"
    alias cd"$i"="cd +$i"
done

# Command Hook (Previous)
function pre_cmd_handler() {
    if [ -z "$__cmd_handler_at_prompt" ]; then return; fi
    unset __cmd_handler_at_prompt

    # Command handling
    if [ "$BASH_COMMAND" == "post_cmd_handler" ]; then
        dirs  # If empty input, print directory stack
    fi
}
trap "pre_cmd_handler" DEBUG

# Command Hook (Post)
function post_cmd_handler() {
    __cmd_handler_at_prompt=1
    # Nothing to do
}
PROMPT_COMMAND="post_cmd_handler"

# aliases for git
alias g='git'
alias gs='git status'
function gg() { git graph --color=always $* | less -EFRSX; }
function gl() { git log --color=always --graph $* | less -EFRX; }
function gla() { git log --color=always --graph --all $* | less -EFRX; }
alias gb='git branch'
alias gbd='git branch -d'
alias gbD='git branch -D'
alias gba='git branch -a'
alias gbc='git branch --show-current'
alias gurl='git remote -v'
alias gd='git diff'
alias gdc='git diff --cached'
alias ga='git add'
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
function gcoF() { b="$*"; git branch -D $b && git checkout -b $b; }
alias gm='git merge'
alias grb='git rebase'
alias gf='git fetch'
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
# Git completion
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
    function filer() { command thunar $@ & &> /dev/null; disown; }
    function zathura() { command zathura $@ & &> /dev/null; disown; }
elif [ $platform == 'Windows' ]; then
    function filer() { command explorer $@ & &> /dev/null; disown; }
    alias w=winpty
fi
alias f=filer

# Synaptics and Game pad
if [ $platform == 'Linux' ]; then
    if [ "`$exist_command synclient`" == 'exist' ]; then
        synclient VertScrollDelta=-30 HorizScrollDelta=-30
        synclient MaxSpeed=2.0 AccelFactor=0.10
    fi
    export SDL_JOYSTICK_DEVICE=/dev/input/js0
    export SDL_GAMECONTROLLERCONFIG='030000008f0e00000300000010010000,GreenAsia Inc.    USB Joystick,platform:Linux,a:b1,b:b2,x:b3,y:b0,back:b8,start:b9,leftstick:b10,rightstick:b11,leftshoulder:b6,rightshoulder:b7,dpup:h0.1,dpdown:h0.4,dpleft:h0.8,dpright:h0.2,leftx:a0,lefty:a1,rightx:a3,righty:a2,lefttrigger:b4,righttrigger:b5,'  # Generated by `controllermap 0`
fi

# Python path
# if [ $platform == 'Linux' ]; then
#     export PYTHONPATH="/usr/local/lib/python3.7/site-packages:$PYTHONPATH"
# fi

# Caffe
# export LD_LIBRARY_PATH=~/Projects/caffe/.build_release/lib:$LD_LIBRARY_PATH
# export PYTHONPATH=~/Projects/caffe/python:$PYTHONPATH

# CUDA
if [ -e /usr/local/cuda ]; then
    export CUDA_HOME=/usr/local/cuda
elif [ -e /opt/cuda ]; then
    export CUDA_HOME=/opt/cuda
fi
if [ "$CUDA_HOME" != "" ]; then
    export CUDA_PATH=$CUDA_HOME
    export CUDA_CUDART_LIBRARY=$CUDA_HOME
    export PATH=$PATH:$CUDA_HOME/bin
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:$CUDA_HOME/lib64
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:$CUDA_HOME/targets/x86_64-linux/lib
fi

# Virtualbox
VBOX_USB=usbfs

# Proxy
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

# Swift-Shader
# export SWIFTSHADER_LIB_PATH="$HOME/tmp/swiftshader/build/Linux"
if [ "$SWIFTSHADER_LIB_PATH" != "" ]; then
    export VK_ICD_FILENAMES=$SWIFTSHADER_LIB_PATH/vk_swiftshader_icd.json
    export LD_LIBRARY_PATH=$SWIFTSHADER_LIB_PATH:$LD_LIBRARY_PATH
fi
