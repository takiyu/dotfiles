export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

xmodmap $HOME/.Xmodmap

source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

exec startxfce4
