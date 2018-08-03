#!/bin/sh

version='3.4'
work_dir=$PWD

cd $work_dir
if [ ! -f opencv ]; then
    git clone https://github.com/opencv/opencv
    cd opencv
    git checkout $version
fi
opencv_dir=$work_dir/opencv

cd $work_dir
if [ ! -f opencv_contrib ]; then
    git clone https://github.com/opencv/opencv_contrib
    cd opencv_contrib
    git checkout $version
fi
contrib_dir=$work_dir/opencv_contrib

cd $opencv_dir
mkdir build
cd build
cmake -DBUILD_DOCS=ON -DBUILD_EXAMPLES=ON -DCMAKE_BUILD_TYPE=RELEASE -DWITH_TBB=ON -DWITH_GTK=ON -DWITH_OPENGL=ON -DWITH_QT=ON -DINSTALL_C_EXAMPLES=ON -DWITH_OPENCL=OFF -DWITH_CUDA=OFF -DWITH_OPENNI=OFF -DWITH_UNICAP=ON -DWITH_V4L=ON -DWITH_XINE=ON -DOPENCV_EXTRA_MODULES_PATH=$contrib_dir/modules ..
make -j4
sudo make install
sudo ldconfig
