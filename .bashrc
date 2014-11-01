export PATH=$HOME/bin:$PATH

export GOPATH=$HOME/Gocode
export GOROOT=/usr/local/go
export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

# export PATH=$HOME/.rbenv/bin:$PATH
# eval "$(rbenv init -)"
#<For Check>
#echo 'eval "$(rbenv init -)"' 

export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u\[\033[01;34m\] \w \$\[\033[00m\] '


# Capsが存在したらキーをリマップ(2回目以降のエラー防止)
if xmodmap -pke | grep Caps;
then
	xmodmap $HOME/.xmodmaprc
fi

# VAIO T Scroll direction setting
xinput --set-button-map 11 1 2 3 5 4 6 7 8 9 10 11 12
