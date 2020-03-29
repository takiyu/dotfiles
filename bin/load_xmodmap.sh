#!/bin/bash

export HOME=/home/takiyu/
export DISPLAY=":0"
export XAUTHORITY=$HOME/.Xauthority

setxkbmap
xmodmap $HOME/.Xmodmap
