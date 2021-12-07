#!/bin/bash
FONT_DIRNAME=~/.local/share/fonts/

# Create font directory
mkdir -p $FONT_DIRNAME

# Copy
cp *.ttc $FONT_DIRNAME
cp *.ttf $FONT_DIRNAME

# Update cache
fc-cache -f -v
