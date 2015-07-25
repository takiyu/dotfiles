export PATH=$HOME/bin:$PATH

# export PATH=$HOME/.rbenv/bin:$PATH
# eval "$(rbenv init -)"
#<For Check>
#echo 'eval "$(rbenv init -)"' 

export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u\[\033[01;34m\] \w \$\[\033[00m\] '

# VAIO T Scroll direction setting
xinput --set-button-map 11 1 2 3 5 4 7 6 8 9 10 11 12

# gksu freq_powersave

export GOPATH=$HOME/Projects/Gocode
export PATH=$GOPATH/bin:$PATH

export PATH=/usr/local/cuda-7.0/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/cuda-7.0/lib64:$LD_LIBRARY_PATH
