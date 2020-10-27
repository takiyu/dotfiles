#!/bin/bash

# Common software
trizen -S neovim neovim-qt
trizen -S linux-headers
trizen -S pkgfile
trizen -S net-tools
trizen -S words
trizen -S w3m
trizen -S nkf lv
trizen -S htop
trizen -S alsa-firmware pavucontrol
trizen -S zathura zathura-pdf-poppler poppler-data
trizen -S evince
trizen -S smplayer mcomix
trizen -S firefox
trizen -S gnome-calculator
trizen -S slack-desktop
trizen -S goldendict
trizen -S simplescreenrecorder
trizen -S chromium chromium-widevine
trizen -S pepper-flash
trizen -S cmigemo-git
trizen -S unzip-iconv
trizen -S virtualbox
trizen -S virtualbox-ext-oracle
trizen -S gimp inkscape
trizen -S sane xsane simple-scan

# Development
trizen -S highlight
trizen -S clang ctags global
trizen -S jdk8-openjdk jdtls
sudo archlinux-java set java-8-openjdk
trizen -S python python-pip python2 python2-pip python-language-server
sudo pip install pyflakes pep8
trizen -S nodejs npm
sudo npm install -g jsxhint
trizen -S texlive-core texlive-langjapanese texlive-latexextra texlive-most
trizen -S texlive-fontsextra texlive-formatsextra texlive-pictures texlive-genericextra
trizen -S opencv
trizen -S paraview
# trizen -S openni2-libfreenect libfeenect2
# trizen -S pcl
trizen -S cmake premake4 premake-git
# trizen -S ocl-icd intel-opencl-runtime opencl-headers

# Fonts
trizen -S ttf-dejavu noto-fonts-cjk
echo "Note: To disable visible zenkaku space,"
echo "      `pkgbuild` edit and add `-z` to `./ricty_generator.sh`"
trizen -S ttf-ricty
trizen -S ttf-liberation
trizen -S noto-fonts-emoji

# Desktop
trizen -S xmonad xmonad-contrib xmobar
trizen -S xfce4 xfce4-goodies xfce4-notifyd xfce4-screenshooter xfce4-taskmanager
trizen -S fcitx fcitx-mozc fcitx-configtool fcitx-gtk2 fcitx-gtk3 fcitx-qt4 fcitx-qt5
trizen -S xscreensaver
trizen -S networkmanager network-manager-applet gnome-keyring
trizen -S libcanberra libcanberra-pulse
trizen -S gvfs gvfs-smb sshfs gvfs-mtp mtpfs
trizen -S xclip

# Android Development
trizen -S android-sdk android-sdk-platform-tools android-sdk-build-tools android-sdk-cmake
trizen -S android-platform android-ndk
trizen -S pidcat
trizen -S android-studio

# Bluetooth
trizen -S xfce4-pulseaudio-plugin
trizen -S pulseaudio-bluetooth
trizen -S blueman
