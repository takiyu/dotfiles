#
# ~/.bash_profile for Linux
#

# $HOME/dotfiles/bin
export PATH=$HOME/dotfiles/bin:$PATH
# $HOME/bin
export PATH=$HOME/bin:$PATH

# Lib
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

# Java
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
export JAVA_AWT_LIBRARY=/usr/lib/jvm/java-8-openjdk/jre/lib/amd64/libawt.so
export JAVA_JVM_LIBRARY=/usr/lib/jvm/java-8-openjdk/jre/lib/amd64/server/libjvm.so
# Java anti-alias
export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

# Microchip
# export PATH=$PATH:/opt/microchip/xc8/v1.33/bin
# export PATH="$PATH:"/opt/microchip/xc32/v1.40/bin""

# ROS
# source /opt/ros/indigo/setup.bash
# source ~/rosbuild_ws/setup.bash

# Duo
# export DUO_SDK=/home/takiyu/rosbuild_ws/package_dir/DUO-Camera-ROS/build/devel/DUOSDK

# Android
export PATH=$PATH:/opt/android-sdk/platform-tools  # platform-tools (adb etc...)
export PATH=$PATH:/opt/android-sdk/tools           # tools (android etc...)
export PATH=$PATH:/opt/android-sdk/tools/bin       # tools (sdkmanager etc...)
export PATH=$PATH:/opt/android-sdk/ndk-bundle      # ndk
export PATH=$PATH:/opt/android-ndk                 # ndk
export ANDROID_HOME=/opt/android-sdk
export ANDROID_NDK=/opt/android-ndk
export ANDROID_NDK_HOME=$ANDROID_NDK

# Golang
export GOPATH=$HOME/Projects/Gocode

# Unity
export PATH=$PATH:/opt/Unity/Editor/

# .bashrc
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi
