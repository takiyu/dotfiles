#!/bin/bash

# utility scripts
dotfiles=$HOME/dotfiles

# Create font directory
FONT_DIRNAME=~/.local/share/fonts/
mkdir -p $FONT_DIRNAME

# Copy
cp dotfiles/fonts/*.ttc $FONT_DIRNAME
cp dotfiles/fonts/*.ttf $FONT_DIRNAME

# Update cache
fc-cache -f -v
