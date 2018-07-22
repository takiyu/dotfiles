# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# utility scripts
dotfiles=$HOME/dotfiles
determ_platform=$dotfiles/utils/determ_platform.sh
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

# auto fix cd path
shopt -s cdspell
# append to the history file, don't overwrite it
shopt -s histappend
# check the window size after each command
shopt -s checkwinsize
# enable **
shopt -s globstar
# Disable beep
set bell-style none

# git prompt
GIT_PS1_SHOWUPSTREAM=1
GIT_PS1_SHOWUNTRACKEDFILES=
GIT_PS1_SHOWSTASHSTATE=
GIT_PS1_SHOWDIRTYSTATE=1
source $git_prompt
source $git_completion

# color prompt
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u\[\033[01;34m\] \w\[\033[01;31m\]$(__git_ps1) \[\033[01;34m\]\$ \[\033[00m\]'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# aliases for ls
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias sl=ls

# aliases for git
alias g='git'
alias gs='git status'
alias gg='git graph'
alias gb='git branch -a'
alias gd='git diff'
alias ga='git add'
alias gau='git add -u'
alias gcm='git commit -m'
alias gco='git checkout'
alias gp='git push'
alias gf='git fetch'
alias gpom='git push origin master'
alias gr='git reset'
alias gR='git reset --hard'

# aliases for editors
alias v=gvim
alias vim=nvim
if [ $platform == 'Linux' ]; then
    function gvim() { command nvim-qt $@ 2> /dev/null; }
    EDITOR=vim
elif [ $platform == 'Windows' ]; then
    function gvim() { command nvim-qt $@ & 2> /dev/null; disown; }
    EDITOR=gvim
fi

# aliases for applications
if [ $platform == 'Linux' ]; then
    function filer() { command thunar $@ &> /dev/null; }
    function zathura() { command zathura $@ & &> /dev/null; }
elif [ $platform == 'Windows' ]; then
    function filer() { command explorer $@ & &> /dev/null; disown; }
fi

# Synaptics
if [ $platform == 'Linux' ]; then
    synclient VertScrollDelta=-30 HorizScrollDelta=-30
    synclient MaxSpeed=2.0 AccelFactor=0.10
fi

# Caffe
# export LD_LIBRARY_PATH=~/Projects/caffe/.build_release/lib:$LD_LIBRARY_PATH
# export PYTHONPATH=~/Projects/caffe/python:$PYTHONPATH

# CUDA
# export PATH=/usr/local/cuda-8.0/bin${PATH:+:${PATH}}
# export LD_LIBRARY_PATH=/usr/local/cuda-8.0/lib64${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
# export CUDA_HOME=/usr/local/cuda
# export CUDA_CUDART_LIBRARY=$CUDA_HOME

# Virtualbox
VBOX_USB=usbfs

# Proxy
#export http_proxy="http://[user]:[pass]@proxyjp.huawei.com:8080"
#export https_proxy="https://[user]:[pass]@proxyjp.huawei.com:8080"
#export ftp_proxy="ftp://[user]:[pass]@proxyjp.huawei.com:8080"
#export NO_PROXY="rnd-dockerhub.huawei.com"
#export GIT_SSL_NO_VERIFY=1
