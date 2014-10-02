#!/bin/sh

#prefixを指定
PREFIX=$HOME/local

wget http://www.lua.org/ftp/lua-5.2.3.tar.gz
tar xzf lua-5.2.3.tar.gz
cd lua-5.2.3
make linux
make install INSTALL_TOP=$PREFIX
cd ..
rm lua-5.2.3 -r
rm lua-5.2.3.tar.gz
