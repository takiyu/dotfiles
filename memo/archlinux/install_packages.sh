#!/bin/bash

# Fonts
pacman -S ttf-dejavu noto-fonts-cjk
echo "Note: To disable visible zenkaku space,"
echo "      `pkgbuild` edit and add `-z` to `./ricty_generator.sh`"
trizen -S ttf-ricty
pacman -S ttf-liberation
trizen -S noto-fonts-emoji

# Desktop
pacman -S xmonad xmonad-contrib xmobar
pacman -S xfce4 xfce4-goodies xfce4-notifyd xfce4-screenshooter xfce4-taskmanager
pacman -S fcitx fcitx-mozc fcitx-configtool fcitx-gtk2 fcitx-gtk3 fcitx-qt4 fcitx-qt5
pacman -S xscreensaver
pacman -S networkmanager network-manager-applet gnome-keyring
pacman -S libcanberra libcanberra-pulse
pacman -S gvfs gvfs-smb sshfs gvfs-mtp mtpfs
pacman -S xclip

# Common software
pacman -S neovim neovim-qt
pacman -S linux-headers
pacman -S pkgfile
pacman -S net-tools
pacman -S words
pacman -S w3m
trizen -S nkf lv
pacman -S htop
pacman -S alsa-firmware pavucontrol
pacman -S zathura zathura-pdf-poppler poppler-data
pacman -S evince
pacman -S smplayer mcomix
pacman -S firefox
pacman -S gnome-calculator
trizen -S slack-desktop
pacman -S goldendict
pacman -S simplescreenrecorder
trizen -S chromium chromium-widevine
trizen -S pepper-flash
trizen -S cmigemo-git
trizen -S unzip-iconv
pacman -S virtualbox
trizen -S virtualbox-ext-oracle
pacman -S gimp inkscape

pacman -S sane xsane simple-scan

# Development
pacman -S highlight
trizen -S clang ctags global
pacman -S jdk8-openjdk jdtls
sudo archlinux-java set java-8-openjdk
pacman -S python python-pip python2 python2-pip python-language-server
sudo pip install pyflakes pep8
pacman -S nodejs npm
sudo npm install -g jsxhint
pacman -S texlive-core texlive-langjapanese texlive-latexextra texlive-most
pacman -S texlive-fontsextra texlive-formatsextra texlive-pictures texlive-genericextra
pacman -S opencv
pacman -S paraview
# trizen -S openni2-libfreenect libfeenect2
# trizen -S pcl
trizen -S cmake premake4 premake-git
# trizen -S ocl-icd intel-opencl-runtime opencl-headers

# Android Development
trizen -S android-sdk android-sdk-platform-tools android-sdk-build-tools android-sdk-cmake
trizen -S android-platform android-ndk
trizen -S pidcat
trizen -S android-studio

# Bluetooth
pacman -S xfce4-pulseaudio-plugin
pacman -S pulseaudio-bluetooth
pacman -S blueman
