#!/bin/sh

rm configure_result.log
cd vim
./configure --enable-multibyte --with-features=huge --disable-selinux --prefix=/usr/local --enable-luainterp=yes --with-luajit --with-lua-prefix=/usr --enable-rubyinterp=yes --enable-pythoninterp=yes --with-python-config-dir=/usr/lib/python2.6/config > ../configure_result.log

cd ..
LANG_LIST='lua ruby python'
for item in $LANG_LIST; do
	echo "> $item :"
	cat configure_result.log | grep $item
	echo \n
done

rm configure_result.log
