export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

xmodmap $HOME/.Xmodmap
xinput set-button-map 8 1 2 3 4 5 6 7 8 9 10 11 2

source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

exec startxfce4
