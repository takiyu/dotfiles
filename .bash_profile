#
# ~/.bash_profile
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

# Microchip
# export PATH=$PATH:/opt/microchip/xc8/v1.33/bin
# export PATH="$PATH:"/opt/microchip/xc32/v1.40/bin""

# ROS
# source /opt/ros/indigo/setup.bash
# source ~/rosbuild_ws/setup.bash

# Duo
# export DUO_SDK=/home/takiyu/rosbuild_ws/package_dir/DUO-Camera-ROS/build/devel/DUOSDK

# Golang
export GOPATH=$HOME/Projects/Gocode

# Android
# export PATH=$PATH:/home/takiyu/Downloads/Android/android-ndk-r10e
# export ANDROID_HOME=/home/takiyu/android-sdks
source /etc/profile.d/android-ndk.sh

# .bashrc
[[ -f ~/.bashrc ]] && . ~/.bashrc
