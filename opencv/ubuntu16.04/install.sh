#!/bin/sh
sudo apt-get update
sudo apt-get install -yV build-essential checkinstall git libfaac-dev libjack-jackd2-dev libmp3lame-dev libopencore-amrnb-dev libopencore-amrwb-dev libsdl1.2-dev libtheora-dev libva-dev libvdpau-dev libvorbis-dev libx11-dev libxfixes-dev texi2html yasm zlib1g-dev
sudo apt-get install -yV ffmpeg x264 libx264-dev

sudo apt-get -yV install libboost-all-dev

sudo apt-get -yV install libqt4-dev 
sudo apt-get -yV install libgtk2.0-dev 
sudo apt-get -yV install pkg-config 

sudo apt-get -yV install opencl-headers 
 
sudo apt-get -yV install libgomp1
 
sudo apt-get -yV install libjpeg-dev
sudo apt-get -yV install libopenjpeg-dev
sudo apt-get -yV install jasper
sudo apt-get -yV install libjasper-dev libjasper-runtime
sudo apt-get -yV install libpng12-dev
sudo apt-get -yV install libpng++-dev libpng3
sudo apt-get -yV install libpnglite-dev libpngwriter0-dev libpngwriter0c2
sudo apt-get -yV install libtiff-dev libtiff-tools pngtools
sudo apt-get -yV install zlib1g-dev zlib1g-dbg
sudo apt-get -yV install v4l2ucp

sudo apt-get -yV install python python3
sudo apt-get -yV install libpython-all-dev
sudo apt-get -yV install libpython3-all-dev
sudo apt-get -yV install autoconf
sudo apt-get -yV install libtbb2 libtbb-dev
sudo apt-get -yV install libeigen2-dev
sudo apt-get -yV install cmake
sudo apt-get -yV install openexr
sudo apt-get -yV install gstreamer-plugins-*
sudo apt-get -yV install freeglut3-dev
sudo apt-get -yV install libglui-dev
sudo apt-get -yV install libavc1394-dev libdc1394-22-dev libdc1394-utils

sudo apt-get -yV install libxine-dev
sudo apt-get -yV install libxvidcore-dev 
sudo apt-get -yV install libva-dev
sudo apt-get -yV install libssl-dev
sudo apt-get -yV install libv4l-dev
sudo apt-get -yV install libvo-aacenc-dev
sudo apt-get -yV install libvo-amrwbenc-dev 
sudo apt-get -yV install libvorbis-dev 
sudo apt-get -yV install libvpx-dev

sudo apt-get -yV install unzip

sudo apt-get -yV install libblas-dev liblapack-dev 
sudo apt-get -yV install libeigen3-dev

version='3.2.0'
cd /tmp
if [ ! -f OpenCV-$version.zip ]; then
    wget http://sourceforge.net/projects/opencvlibrary/files/opencv-unix/$version/opencv-$version.zip
fi 

sudo rm -rf opencv-$version
unzip /tmp/opencv-$version
cd opencv-$version
cmake -DBUILD_DOCS=ON -DBUILD_EXAMPLES=ON -DCMAKE_BUILD_TYPE=RELEASE -DWITH_TBB=ON -DWITH_GTK=ON -DWITH_OPENGL=ON -DWITH_QT=ON -DINSTALL_C_EXAMPLES=ON -DWITH_OPENCL=OFF -DWITH_CUDA=OFF -DWITH_OPENNI=OFF -DWITH_UNICAP=ON -DWITH_V4L=ON -DWITH_XINE=ON  .
# cmake -DBUILD_DOCS=ON -DBUILD_EXAMPLES=ON -DCMAKE_BUILD_TYPE=RELEASE -DWITH_TBB=ON -DWITH_GTK=ON -DWITH_OPENGL=ON -DWITH_QT=ON -DINSTALL_C_EXAMPLES=ON -DWITH_OPENCL=OFF -DWITH_CUDA=ON -DWITH_OPENNI=ON -DWITH_UNICAP=ON -DWITH_V4L=ON -DWITH_XINE=ON -D CUDA_GENERATION=Kepler .
make -j4
sudo make install
sudo ldconfig
