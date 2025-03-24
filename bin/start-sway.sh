#!/bin/bash

# Session
export XDG_SESSION_TYPE=wayland
export XDG_SESSION_DESKTOP=sway
export XDG_CURRENT_DESKTOP=sway

# Wayland stuff
export MOZ_ENABLE_WAYLAND=1
export QT_QPA_PLATFORM="wayland;xcb"
export SDL_VIDEODRIVER=wayland
export _JAVA_AWT_WM_NONREPARENTING=1

# Cursor
export WLR_NO_HARDWARE_CURSORS=1

# Fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

exec bash -l -c "sway --unsupported-gpu"
