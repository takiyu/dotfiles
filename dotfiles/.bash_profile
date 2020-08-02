#
# ~/.bash_profile
#

# utility scripts
dotfiles=$HOME/dotfiles
determ_platform=$dotfiles/utils/determ_platform.sh

# determine the platform
platform=`$determ_platform`

# Local path
export PATH=$HOME/dotfiles/bin:$PATH  # $HOME/dotfiles/bin
export PATH=$HOME/bin:$PATH           # $HOME/bin
export PATH=$HOME/local/bin:$PATH     # $HOME/local/bin
export PATH=$HOME/.local/bin:$PATH    # $HOME/.local/bin
# Local lib
export LD_LIBRARY_PATH=$HOME/local/lib:$LD_LIBRARY_PATH   # $HOME/local/bin
export LD_LIBRARY_PATH=$HOME/.local/lib:$LD_LIBRARY_PATH  # $HOME/.local/bin

if [ $platform == 'Linux' ]; then
    # Linux setup

    # Lib
    export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH  # /usr/local/lib

    # IME
    export GTK_IM_MODULE=fcitx
    export QT_IM_MODULE=fcitx
    export XMODIFIERS=@im=fcitx

    # Java
    export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
    export JAVA_AWT_LIBRARY=/usr/lib/jvm/java-8-openjdk/jre/lib/amd64/libawt.so
    export JAVA_JVM_LIBRARY=/usr/lib/jvm/java-8-openjdk/jre/lib/amd64/server/libjvm.so
    # Java anti-alias
    export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
    # For xmonad
    export _JAVA_AWT_WM_NONREPARENTING=1

    # Microchip
    # export PATH=$PATH:/opt/microchip/xc8/v1.33/bin
    # export PATH="$PATH:"/opt/microchip/xc32/v1.40/bin""

    # ROS
    # source /opt/ros/indigo/setup.bash
    # source ~/rosbuild_ws/setup.bash

    # Android
    if [ -e /opt/android-sdk ]; then
        export ANDROID_HOME=/opt/android-sdk
        export ANDROID_NDK=/opt/android-ndk
        export ANDROID_NDK_HOME=$ANDROID_NDK
        export PATH=$PATH:$ANDROID_HOME/platform-tools  # platform-tools (adb etc...)
        export PATH=$PATH:$ANDROID_HOME/tools           # tools (android etc...)
        export PATH=$PATH:$ANDROID_HOME/tools/bin       # tools (sdkmanager etc...)
        export PATH=$PATH:$ANDROID_NDK                  # ndk
    fi

    # Golang
    export GOPATH=$HOME/Projects/Gocode

    # Unity
    if [ -e /opt/Unity/Editor ]; then
        export PATH=$PATH:/opt/Unity/Editor/
    fi

elif [ $platform == 'Windows' ]; then
    # Windows setup
    :  # NOP
fi

# .bashrc
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi
