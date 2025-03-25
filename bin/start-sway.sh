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
export GTK_IM_MODULE=fcitx5
export QT_IM_MODULE=fcitx5
export QT_IM_MODULES="wayland;fcitx5"
export XMODIFIERS=@im=fcitx5
export SDL_IM_MODULE=fcitx5
export GLFW_IM_MODULE=fcitx5

# Start sway on login shell (loading .bash_profile)
exec bash -l -c "sway --unsupported-gpu"
