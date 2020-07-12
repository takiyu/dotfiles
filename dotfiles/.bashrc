# ~/.bashrc: executed by bash(1) for non-login shells.

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
if [ $platform == 'Linux' ]; then
    GIT_PS1_SHOWUPSTREAM=1
    GIT_PS1_SHOWUNTRACKEDFILES=
    GIT_PS1_SHOWSTASHSTATE=
    GIT_PS1_SHOWDIRTYSTATE=
    source $git_prompt
    source $git_completion
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
function gg() { git graph --color=always $* | less -EFRSX; }
function gl() { git log --color=always --graph $* | less -EFRX; }
function gla() { git log --color=always --graph --all $* | less -EFRX; }
alias gb='git branch -a'
alias gd='git diff'
alias gdc='git diff --cached'
alias ga='git add'
alias gau='git add -u'
alias gcm='git commit'
function gcmm() { git commit -m "$*"; }
alias gcma='git commit --amend'
function gcmma() { git commit --amend -m "$*"; }
function gcmam() { git commit --amend -m "$*"; }
alias gco='git checkout'
alias gf='git fetch'
alias gp='git pull'
alias gP='git push'
alias gpo='git pull origin'
alias gPo='git push origin'
alias gpom='git pull origin master'
alias gPom='git push origin master'
alias gr='git reset'
alias gR='git reset --hard'
function gro() { git reset origin/"$*"; }
function gRo() { git reset origin/"$*" --hard; }
alias grom='git reset origin/master'
alias gRom='git reset --hard origin/master'

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

# aliases for cd
alias cd-="cd -"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias cd..="cd .."
alias cd...="cd ../.."
alias cd....="cd ../../.."
alias cd.....="cd ../../../.."
alias cd......="cd ../../../../.."

# aliases for editors
alias v=gvim
if [ $platform == 'Linux' ]; then
    alias vim=nvim
    function gvim() { command nvim-qt $@ 2> /dev/null; }
    EDITOR=vim
elif [ $platform == 'Windows' ]; then
    alias vim=nvim
    function gvim() { command nvim-qt $@ & 2> /dev/null; disown; }
    EDITOR=gvim
fi
alias vimdiff="vim -d"
alias gvimdiff="gvim -- -d"

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
if [ -e /opt/cuda ]; then
    export CUDA_HOME=/opt/cuda
    export CUDA_PATH=$CUDA_HOME
    export CUDA_CUDART_LIBRARY=$CUDA_HOME
    export PATH=$PATH:$CUDA_HOME/bin
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:$CUDA_HOME/lib64
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:$CUDA_HOME/targets/x86_64-linux/lib
fi

# Virtualbox
VBOX_USB=usbfs

# Proxy
# USER='<Set user here>'
# PASS='<Set pass here>'
# export http_proxy="http://$USER:$PASS@proxyjp.huawei.com:8080"
# export https_proxy="https://$USER:$PASS@proxyjp.huawei.com:8080"
# export ftp_proxy="ftp://$USER:$PASS@proxyjp.huawei.com:8080"
# export http_proxy="http://$USER:$PASS@localhost:8888"
# export https_proxy="https://$USER:$PASS@localhost:8888"
# export ftp_proxy="ftp://$USER:$PASS@localhost:8888"
# export NO_PROXY="huawei.com"
# export GIT_SSL_NO_VERIFY=1

export http_proxy=$HTTP_PROXY
export https_proxy=$HTTPS_PROXY
export ftp_proxy=$FTP_PROXY
export no_proxy=$NO_PROXY
